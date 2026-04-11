#!/usr/bin/env python3
"""
scripts/lt_import.py

Import a LibraryThing JSON export into a SQLite database.

Usage:
    python3 lt_import.py [--json FILE] [--db FILE]

Defaults:
    --json  ~/ExternalBrain/librarything_force_*.json  (most recent match)
    --db    ~/.crichton/data/books.db

The import is idempotent: re-running after a fresh LT export safely updates
existing records and adds new ones. Junction tables are rebuilt per-book.
"""

import argparse
import glob
import json
import os
import re
import sqlite3
import sys
from pathlib import Path


# ---------------------------------------------------------------------------
# Schema
# ---------------------------------------------------------------------------

SCHEMA = """
CREATE TABLE IF NOT EXISTS books (
    id              INTEGER PRIMARY KEY,
    lt_id           TEXT    NOT NULL UNIQUE,
    workcode        TEXT,
    title           TEXT    NOT NULL,
    sort_title      TEXT    NOT NULL,
    primary_author  TEXT,
    isbn            TEXT,
    year            INTEGER,
    publication     TEXT,
    pages           INTEGER,
    rating          REAL,
    copies          INTEGER DEFAULT 1,
    entry_date      TEXT,
    format          TEXT,
    language        TEXT,
    public          INTEGER DEFAULT 1
);

CREATE TABLE IF NOT EXISTS authors (
    id      INTEGER PRIMARY KEY,
    name_lf TEXT NOT NULL UNIQUE   -- "Last, First" form
);

CREATE TABLE IF NOT EXISTS book_authors (
    book_id   INTEGER NOT NULL REFERENCES books(id) ON DELETE CASCADE,
    author_id INTEGER NOT NULL REFERENCES authors(id),
    role      TEXT,
    position  INTEGER NOT NULL DEFAULT 0,  -- 0 = primary
    PRIMARY KEY (book_id, author_id)
);

CREATE TABLE IF NOT EXISTS tags (
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS book_tags (
    book_id INTEGER NOT NULL REFERENCES books(id) ON DELETE CASCADE,
    tag_id  INTEGER NOT NULL REFERENCES tags(id),
    PRIMARY KEY (book_id, tag_id)
);

CREATE TABLE IF NOT EXISTS collections (
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS book_collections (
    book_id       INTEGER NOT NULL REFERENCES books(id) ON DELETE CASCADE,
    collection_id INTEGER NOT NULL REFERENCES collections(id),
    PRIMARY KEY (book_id, collection_id)
);

CREATE TABLE IF NOT EXISTS genres (
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS book_genres (
    book_id  INTEGER NOT NULL REFERENCES books(id) ON DELETE CASCADE,
    genre_id INTEGER NOT NULL REFERENCES genres(id),
    PRIMARY KEY (book_id, genre_id)
);

CREATE TABLE IF NOT EXISTS series (
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS book_series (
    book_id   INTEGER NOT NULL REFERENCES books(id) ON DELETE CASCADE,
    series_id INTEGER NOT NULL REFERENCES series(id),
    PRIMARY KEY (book_id, series_id)
);

CREATE VIRTUAL TABLE IF NOT EXISTS books_fts USING fts5(
    title,
    primary_author,
    content='books',
    content_rowid='id'
);
"""

INDEXES = """
CREATE INDEX IF NOT EXISTS idx_books_primary_author ON books(primary_author);
CREATE INDEX IF NOT EXISTS idx_books_year            ON books(year);
CREATE INDEX IF NOT EXISTS idx_books_rating          ON books(rating);
CREATE INDEX IF NOT EXISTS idx_book_authors_author   ON book_authors(author_id);
CREATE INDEX IF NOT EXISTS idx_book_tags_tag         ON book_tags(tag_id);
CREATE INDEX IF NOT EXISTS idx_book_series_series    ON book_series(series_id);
"""


# ---------------------------------------------------------------------------
# Field extraction helpers
# ---------------------------------------------------------------------------

_ARTICLE_RE = re.compile(r'^(the|a|an)\s+', re.IGNORECASE)


def sort_title(title: str) -> str:
    """Strip leading English articles for alphabetic sorting."""
    return _ARTICLE_RE.sub('', title).strip()


def extract_isbn(isbn_field) -> str | None:
    """Prefer ISBN-13 (key '2') over ISBN-10 (key '0')."""
    if not isbn_field:
        return None
    if isinstance(isbn_field, dict):
        return isbn_field.get('2') or isbn_field.get('0')
    if isinstance(isbn_field, str):
        return isbn_field or None
    return None


def extract_year(date_field) -> int | None:
    """Extract a four-digit year from the date string."""
    if not date_field:
        return None
    m = re.search(r'\b(\d{4})\b', str(date_field))
    return int(m.group(1)) if m else None


def extract_pages(pages_field) -> int | None:
    """Return the main (Arabic) page count from strings like 'xxxvii; 329 '."""
    if not pages_field:
        return None
    # Find the last integer in the string (after any Roman-numeral prefix)
    nums = re.findall(r'\d+', str(pages_field))
    return int(nums[-1]) if nums else None


def extract_format(format_field) -> str | None:
    """Return the text of the first format entry."""
    if not format_field:
        return None
    if isinstance(format_field, list) and format_field:
        entry = format_field[0]
        if isinstance(entry, dict):
            return entry.get('text')
        return str(entry)
    if isinstance(format_field, str):
        return format_field or None
    return None


def extract_language(language_field) -> str | None:
    """Return the first language name."""
    if not language_field:
        return None
    if isinstance(language_field, list) and language_field:
        return language_field[0]
    if isinstance(language_field, str):
        return language_field or None
    return None


def extract_bool(val) -> int:
    if isinstance(val, bool):
        return int(val)
    if isinstance(val, str):
        return 1 if val.lower() == 'true' else 0
    return 1


def coerce_int(val) -> int | None:
    try:
        return int(val) if val is not None else None
    except (ValueError, TypeError):
        return None


# ---------------------------------------------------------------------------
# Database helpers
# ---------------------------------------------------------------------------

def get_or_create(cur: sqlite3.Cursor, table: str, col: str, value: str) -> int:
    cur.execute(f"INSERT OR IGNORE INTO {table} ({col}) VALUES (?)", (value,))
    cur.execute(f"SELECT id FROM {table} WHERE {col} = ?", (value,))
    return cur.fetchone()[0]


def upsert_book(cur: sqlite3.Cursor, book: dict) -> int:
    """Insert or update the core books row. Returns the internal book id."""
    lt_id = str(book['books_id'])
    title = book.get('title', '').strip()
    stitle = sort_title(title)
    primary_author = book.get('primaryauthor') or None
    isbn = extract_isbn(book.get('isbn'))
    year = extract_year(book.get('date'))
    publication = book.get('publication') or None
    pages = extract_pages(book.get('pages'))
    rating = book.get('rating')
    copies = coerce_int(book.get('copies')) or 1
    entry_date = book.get('entrydate') or None
    fmt = extract_format(book.get('format'))
    language = extract_language(book.get('language'))
    public = extract_bool(book.get('public', True))
    workcode = book.get('workcode') or None

    cur.execute("""
        INSERT INTO books
            (lt_id, workcode, title, sort_title, primary_author,
             isbn, year, publication, pages, rating,
             copies, entry_date, format, language, public)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT(lt_id) DO UPDATE SET
            workcode       = excluded.workcode,
            title          = excluded.title,
            sort_title     = excluded.sort_title,
            primary_author = excluded.primary_author,
            isbn           = excluded.isbn,
            year           = excluded.year,
            publication    = excluded.publication,
            pages          = excluded.pages,
            rating         = excluded.rating,
            copies         = excluded.copies,
            entry_date     = excluded.entry_date,
            format         = excluded.format,
            language       = excluded.language,
            public         = excluded.public
    """, (lt_id, workcode, title, stitle, primary_author,
          isbn, year, publication, pages, rating,
          copies, entry_date, fmt, language, public))

    cur.execute("SELECT id FROM books WHERE lt_id = ?", (lt_id,))
    return cur.fetchone()[0]


def rebuild_junction(cur: sqlite3.Cursor, book_id: int,
                     junction_table: str, value_table: str, value_col: str,
                     fk_col: str, values: list[str]) -> None:
    """Wipe and rebuild a junction table for one book.
    FK_COL is the column name in the junction table for the foreign key
    (e.g. 'tag_id', 'series_id')."""
    cur.execute(f"DELETE FROM {junction_table} WHERE book_id = ?", (book_id,))
    for v in values:
        if not v:
            continue
        ref_id = get_or_create(cur, value_table, value_col, v)
        cur.execute(
            f"INSERT OR IGNORE INTO {junction_table} (book_id, {fk_col}) VALUES (?,?)",
            (book_id, ref_id)
        )


def rebuild_authors(cur: sqlite3.Cursor, book_id: int, book: dict) -> None:
    cur.execute("DELETE FROM book_authors WHERE book_id = ?", (book_id,))
    authors = book.get('authors') or []
    for pos, author in enumerate(authors):
        if isinstance(author, dict):
            name_lf = author.get('lf') or author.get('fl') or ''
        else:
            name_lf = str(author)
        if not name_lf.strip():
            continue
        author_id = get_or_create(cur, 'authors', 'name_lf', name_lf.strip())
        cur.execute(
            "INSERT OR IGNORE INTO book_authors (book_id, author_id, position) VALUES (?,?,?)",
            (book_id, author_id, pos)
        )


# ---------------------------------------------------------------------------
# FTS rebuild
# ---------------------------------------------------------------------------

def rebuild_fts(con: sqlite3.Connection) -> None:
    con.execute("INSERT INTO books_fts(books_fts) VALUES('rebuild')")


# ---------------------------------------------------------------------------
# Main import
# ---------------------------------------------------------------------------

def find_json(pattern: str) -> str:
    """Expand glob and return the most recently modified match."""
    matches = glob.glob(os.path.expanduser(pattern))
    if not matches:
        raise FileNotFoundError(f"No file matching: {pattern}")
    return max(matches, key=os.path.getmtime)


def import_library(json_path: str, db_path: str, verbose: bool = True) -> None:
    if verbose:
        print(f"JSON: {json_path}")
        print(f"DB:   {db_path}")

    Path(db_path).parent.mkdir(parents=True, exist_ok=True)

    with open(json_path, encoding='utf-8') as f:
        data = json.load(f)

    con = sqlite3.connect(db_path)
    con.execute("PRAGMA foreign_keys = ON")
    con.execute("PRAGMA journal_mode = WAL")

    with con:
        for stmt in SCHEMA.split(';'):
            s = stmt.strip()
            if s:
                con.execute(s)
        for stmt in INDEXES.split(';'):
            s = stmt.strip()
            if s:
                con.execute(s)

    inserted = updated = 0

    with con:
        cur = con.cursor()
        for lt_key, book in data.items():
            lt_id = str(book.get('books_id', lt_key))
            cur.execute("SELECT id FROM books WHERE lt_id = ?", (lt_id,))
            existed = cur.fetchone() is not None

            book_id = upsert_book(cur, book)

            # Authors
            rebuild_authors(cur, book_id, book)

            # Tags
            rebuild_junction(cur, book_id,
                             'book_tags', 'tags', 'name', 'tag_id',
                             [t for t in (book.get('tags') or []) if t])

            # Collections
            rebuild_junction(cur, book_id,
                             'book_collections', 'collections', 'name', 'collection_id',
                             [c for c in (book.get('collections') or []) if c])

            # Genres
            rebuild_junction(cur, book_id,
                             'book_genres', 'genres', 'name', 'genre_id',
                             [g for g in (book.get('genre') or []) if g])

            # Series
            rebuild_junction(cur, book_id,
                             'book_series', 'series', 'name', 'series_id',
                             [s for s in (book.get('series') or []) if s])

            if existed:
                updated += 1
            else:
                inserted += 1

    if verbose:
        print(f"Books: {inserted} inserted, {updated} updated")

    with con:
        rebuild_fts(con)
        if verbose:
            print("FTS index rebuilt.")

    con.close()


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    default_json = '~/ExternalBrain/librarything_force_*.json'
    default_db   = '~/.crichton/data/books.db'

    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--json', default=default_json,
                        help=f'LibraryThing JSON export (glob OK, default: {default_json})')
    parser.add_argument('--db', default=default_db,
                        help=f'SQLite database path (default: {default_db})')
    parser.add_argument('--quiet', action='store_true')
    args = parser.parse_args()

    try:
        json_path = find_json(args.json)
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    import_library(json_path, os.path.expanduser(args.db), verbose=not args.quiet)


if __name__ == '__main__':
    main()
