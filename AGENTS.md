# Workspace Agent Instructions

## Projects

- **Crichton** (`crichton/`) — Secure SBCL Common Lisp AI agent daemon. See `AGENT_SPEC.md` for full design spec, `crichton.asd` for module structure.

## Workflow Practices

- **Session startup**: Read this file (AGENTS.md) and MEMORY.md at the start of each session to understand project context and user preferences. Run `bd ready` to see what work is outstanding.
- **Issue tracking**: Use BEADS (`bd` command) for all issue tracking. Beads live in `.beads/` (prefix: `cricht-`). File new beads for bugs, feature requests, style warnings, and cleanup items. Do not create ISSUES.md or TODO files.
- **Documentation**: The user prefers Emacs org-mode files (.org) over Markdown files (.md).  Use org-mode format instead (see https://orgmode.org/ for information on this format)
- **Changelog**: When completing user-visible features or bug fixes, add an entry to the "Unreleased" section of `CHANGELOG.org`. Focus on what changed from the user's perspective, not implementation details. Internal refactorings don't need changelog entries (git log is sufficient).
- **Git commits**: Commit changes to git as you go. Every meaningful change gets a commit. The user expects this and will call you out for forgetting. Do NOT add "Co-Authored-By" lines to commit messages.
- **Binary rebuild**: Rebuild the client binary via `save-lisp-and-die` at suitable stopping points — after finishing a feature implementation or fixing a bug. The daemon starts fresh via systemd (no binary needed).
  - **Client binary** (symlinked from `~/bin/crichton-client`):
    ```
    sbcl --noinform --non-interactive \
      --eval '(require :asdf)' \
      --eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
      --eval '(asdf:load-system :crichton-client)' \
      --eval "(sb-ext:save-lisp-and-die #p\"/home/gjbroom/devel/crichton/crichton-client\" :toplevel #'crichton-client:main :executable t :compression t)"
    ```

## Crichton Development Notes

- Phases 1-4 are complete: daemon skeleton, WASM skill runner, encrypted credentials/sessions, LLM integration (Anthropic), agent loop with tool use.
- Microkernel architecture: daemon starts fresh via systemd (`assets/crichton.service`), client connects via `daemon.sock` RPC. No save-lisp-and-die for the daemon.
- The daemon RPC server listens on `~/.crichton/daemon.sock`. Chat, status, stop ops.
- `crichton-client` is a minimal binary (shasht + sb-bsd-sockets only, no cl+ssl/wasmtime).
- 6 built-in skills: weather, system-info, timing, scheduler, RSS, token-usage (general-purpose metering).
- Built-in skills follow a dual interface: plist-returning function for programmatic use, formatted report function for humans.
- **Hot-loading** (preferred for development): After code changes, reload via Swank (SLIME/SLY `M-x slime-connect` to port 4005):
  ```lisp
  (asdf:load-system :crichton :force t)   ; Full reload
  (load "/path/to/file.lisp")              ; Single file
  ```
  This avoids slow stop/rebuild/start cycles. Use this for skill development, bug fixes, tool registration. After hot-loading `daemon/rpc-server.lisp` changes, the RPC server should be restarted.
- TOML config values come through as strings but defaults use keywords. Normalize in consumers.
- Libraries: dexador, shasht, cl-toml, ironclad, log4cl, bordeaux-threads, cffi, hunchentoot, swank-client, usocket, xmls.
- Config lives at `~/.crichton/config.toml`. Default model: `claude-sonnet-4-20250514`.
- API key stored in age-encrypted credential store as `anthropic-api-key` with field `:api-key`.

## Build & Check

- **Load/compile check**: `sbcl --noinform --non-interactive --eval '(require :asdf)' --eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' --eval '(asdf:load-system :crichton :force t)'`
- Target: zero STYLE-WARNING conditions from `(asdf:load-system :crichton :force t)`. Fix all warnings as encountered (boy-scout rule).

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
