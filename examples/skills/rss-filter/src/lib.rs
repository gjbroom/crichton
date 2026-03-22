// Unit tests run on the host (not WASM) so we re-enable std for the test build.
#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::{format, string::String, vec, vec::Vec};
use serde::{Deserialize, Serialize};

// --- WASM memory management (excluded from test builds) ---

#[cfg(not(test))]
mod wasm_support {
    use core::panic::PanicInfo;
    use core::sync::atomic::{AtomicUsize, Ordering};

    const PAGE_SIZE: usize = 65536;
    static HEAP_POS: AtomicUsize = AtomicUsize::new(0);

    pub struct BumpAllocator;

    unsafe impl alloc::alloc::GlobalAlloc for BumpAllocator {
        unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
            let size = layout.size();
            let align = layout.align();

            loop {
                let current = HEAP_POS.load(Ordering::Relaxed);
                let start = if current == 0 {
                    let pages = core::arch::wasm32::memory_grow(0, 1);
                    if pages == usize::MAX {
                        return core::ptr::null_mut();
                    }
                    pages * PAGE_SIZE
                } else {
                    current
                };

                let aligned = (start + align - 1) & !(align - 1);
                let end = aligned + size;
                let page_end = (start / PAGE_SIZE + 1) * PAGE_SIZE;

                if end > page_end {
                    let pages_needed = (end - page_end + PAGE_SIZE - 1) / PAGE_SIZE;
                    if core::arch::wasm32::memory_grow(0, pages_needed) == usize::MAX {
                        return core::ptr::null_mut();
                    }
                }

                if HEAP_POS
                    .compare_exchange(current, end, Ordering::Relaxed, Ordering::Relaxed)
                    .is_ok()
                {
                    return aligned as *mut u8;
                }
            }
        }

        unsafe fn dealloc(&self, _ptr: *mut u8, _layout: core::alloc::Layout) {}
    }

    #[global_allocator]
    pub static ALLOCATOR: BumpAllocator = BumpAllocator;

    #[panic_handler]
    fn panic(_info: &PanicInfo) -> ! {
        core::arch::wasm32::unreachable()
    }

    #[link(wasm_import_module = "env")]
    extern "C" {
        pub fn log(level: i32, ptr: *const u8, len: usize);
    }
}

/// Write a log message via the WASM host. No-op in unit tests.
fn host_log(level: i32, msg: &str) {
    #[cfg(not(test))]
    unsafe {
        wasm_support::log(level, msg.as_ptr(), msg.len());
    }
    #[cfg(test)]
    {
        let _ = (level, msg);
    }
}

/// Exported alloc/dealloc for the JSON-through-memory ABI.
#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn alloc(len: usize) -> *mut u8 {
    let layout = core::alloc::Layout::from_size_align(len, 1).unwrap();
    unsafe { alloc::alloc::alloc(layout) }
}

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, len: usize) {
    let layout = core::alloc::Layout::from_size_align(len, 1).unwrap();
    unsafe { alloc::alloc::dealloc(ptr, layout) }
}

// --- Input validation limits (cricht-5z9) ---

const MAX_ITEMS: usize = 1_000;
const MAX_KEYWORDS: usize = 100;
const MAX_KEYWORD_LEN: usize = 200;
const MAX_ITEM_TEXT_LEN: usize = 100_000;

// --- Data structures ---

#[derive(Deserialize)]
struct FilterParams {
    items: Vec<RssItem>,
    keywords: Vec<String>,
    match_mode: Option<String>,
    search_fields: Option<Vec<String>>,
    case_sensitive: Option<bool>,
}

#[derive(Deserialize)]
struct RssItem {
    id: String,
    title: String,
    description: Option<String>,
    content: Option<String>,
    link: Option<String>,
    published: Option<String>,
    feed_name: Option<String>,
}

#[derive(Serialize)]
struct MatchedItem {
    id: String,
    title: String,
    description: String,
    link: String,
    published: String,
    feed_name: String,
    matched_keywords: Vec<String>,
}

#[derive(Serialize)]
struct FilterResult {
    matches: Vec<MatchedItem>,
    statistics: Statistics,
}

#[derive(Serialize)]
struct Statistics {
    items_scanned: usize,
    items_matched: usize,
}

// --- Input validation ---

/// Validate filter parameters against DoS limits.
/// Returns Ok(()) when all limits are satisfied, Err(description) otherwise.
fn validate_params(params: &FilterParams) -> Result<(), String> {
    if params.items.len() > MAX_ITEMS {
        return Err(format!(
            "too many items: {} (max {})",
            params.items.len(),
            MAX_ITEMS
        ));
    }
    if params.keywords.len() > MAX_KEYWORDS {
        return Err(format!(
            "too many keywords: {} (max {})",
            params.keywords.len(),
            MAX_KEYWORDS
        ));
    }
    for kw in &params.keywords {
        if kw.len() > MAX_KEYWORD_LEN {
            return Err(format!(
                "keyword too long: {} bytes (max {})",
                kw.len(),
                MAX_KEYWORD_LEN
            ));
        }
    }
    for item in &params.items {
        if item.title.len() > MAX_ITEM_TEXT_LEN {
            return Err(format!(
                "item title too long: {} bytes (max {})",
                item.title.len(),
                MAX_ITEM_TEXT_LEN
            ));
        }
        if let Some(desc) = &item.description {
            if desc.len() > MAX_ITEM_TEXT_LEN {
                return Err(format!(
                    "item description too long: {} bytes (max {})",
                    desc.len(),
                    MAX_ITEM_TEXT_LEN
                ));
            }
        }
        if let Some(content) = &item.content {
            if content.len() > MAX_ITEM_TEXT_LEN {
                return Err(format!(
                    "item content too long: {} bytes (max {})",
                    content.len(),
                    MAX_ITEM_TEXT_LEN
                ));
            }
        }
    }
    Ok(())
}

// --- Filtering logic ---

fn to_lowercase(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        for lc in c.to_lowercase() {
            out.push(lc);
        }
    }
    out
}

fn field_contains(text: &str, keyword: &str, case_sensitive: bool) -> bool {
    if case_sensitive {
        text.contains(keyword)
    } else {
        let lower_text = to_lowercase(text);
        let lower_kw = to_lowercase(keyword);
        lower_text.contains(lower_kw.as_str())
    }
}

fn check_item_match(
    item: &RssItem,
    keywords: &[String],
    match_mode: &str,
    search_fields: &[String],
    case_sensitive: bool,
) -> Vec<String> {
    let mut matched: Vec<String> = Vec::new();

    for keyword in keywords {
        let mut found = false;
        for field in search_fields {
            let text: Option<&str> = match field.as_str() {
                "title" => Some(item.title.as_str()),
                "description" => item.description.as_deref(),
                "content" => item.content.as_deref(),
                _ => None,
            };
            if let Some(t) = text {
                if field_contains(t, keyword.as_str(), case_sensitive) {
                    found = true;
                    break;
                }
            }
        }
        if found {
            matched.push(keyword.clone());
        }
    }

    match match_mode {
        "all" => {
            if matched.len() == keywords.len() {
                matched
            } else {
                Vec::new()
            }
        }
        _ => matched, // "any" or default
    }
}

// --- Entry point ---

#[no_mangle]
pub extern "C" fn filter_items(
    params_ptr: *const u8,
    params_len: usize,
    out_ptr: *mut u8,
    out_len: usize,
) -> i32 {
    let input = unsafe { core::slice::from_raw_parts(params_ptr, params_len) };

    let params: FilterParams = match serde_json::from_slice(input) {
        Ok(p) => p,
        Err(_) => {
            host_log(3, "filter_items: failed to parse input JSON");
            return 1;
        }
    };

    if let Err(e) = validate_params(&params) {
        host_log(3, &format!("filter_items: validation error: {}", e));
        return 4;
    }

    let match_mode = params.match_mode.as_deref().unwrap_or("any");
    let default_fields = vec![String::from("title"), String::from("description")];
    let search_fields = params.search_fields.as_deref().unwrap_or(&default_fields);
    let case_sensitive = params.case_sensitive.unwrap_or(false);

    let items_scanned = params.items.len();
    let mut matches: Vec<MatchedItem> = Vec::new();

    for item in &params.items {
        let matched_keywords =
            check_item_match(item, &params.keywords, match_mode, search_fields, case_sensitive);
        if !matched_keywords.is_empty() {
            matches.push(MatchedItem {
                id: item.id.clone(),
                title: item.title.clone(),
                description: item.description.clone().unwrap_or_default(),
                link: item.link.clone().unwrap_or_default(),
                published: item.published.clone().unwrap_or_default(),
                feed_name: item.feed_name.clone().unwrap_or_default(),
                matched_keywords,
            });
        }
    }

    let items_matched = matches.len();

    host_log(
        1,
        &format!(
            "filter_items: scanned={}, matched={}",
            items_scanned, items_matched
        ),
    );

    let result = FilterResult {
        matches,
        statistics: Statistics {
            items_scanned,
            items_matched,
        },
    };

    let output = match serde_json::to_vec(&result) {
        Ok(v) => v,
        Err(_) => {
            host_log(3, "filter_items: failed to serialize output");
            return 3;
        }
    };

    if output.len() > out_len {
        host_log(
            3,
            &format!(
                "filter_items: output {} bytes exceeds buffer {} bytes",
                output.len(),
                out_len
            ),
        );
        return 2;
    }

    unsafe {
        core::ptr::copy_nonoverlapping(output.as_ptr(), out_ptr, output.len());
    }

    0
}

// --- Unit tests (cricht-lli) ---

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;

    /// Build a minimal RssItem for testing.
    fn make_item(title: &str, description: Option<&str>, content: Option<&str>) -> RssItem {
        RssItem {
            id: "test-id".to_string(),
            title: title.to_string(),
            description: description.map(|s| s.to_string()),
            content: content.map(|s| s.to_string()),
            link: None,
            published: None,
            feed_name: None,
        }
    }

    fn fields(names: &[&str]) -> Vec<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    fn kws(words: &[&str]) -> Vec<String> {
        words.iter().map(|s| s.to_string()).collect()
    }

    // --- check_item_match tests ---

    #[test]
    fn test_any_match() {
        let item = make_item("Rust and WASM", None, None);
        let result = check_item_match(&item, &kws(&["Rust", "Python"]), "any", &fields(&["title"]), false);
        assert_eq!(result, kws(&["Rust"]));
    }

    #[test]
    fn test_all_match() {
        let item = make_item("Rust and WASM together", None, None);
        let result = check_item_match(&item, &kws(&["Rust", "WASM"]), "all", &fields(&["title"]), false);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_all_match_fails() {
        // "all" mode: Python is absent, so nothing should match.
        let item = make_item("Only Rust here", None, None);
        let result = check_item_match(&item, &kws(&["Rust", "Python"]), "all", &fields(&["title"]), false);
        assert!(result.is_empty());
    }

    #[test]
    fn test_case_insensitive() {
        let item = make_item("Hello World", None, None);
        let result = check_item_match(&item, &kws(&["hello"]), "any", &fields(&["title"]), false);
        assert_eq!(result, kws(&["hello"]));
    }

    #[test]
    fn test_case_sensitive() {
        // Lowercase "hello" must not match title-case "Hello" in case-sensitive mode.
        let item = make_item("Hello World", None, None);
        let result = check_item_match(&item, &kws(&["hello"]), "any", &fields(&["title"]), true);
        assert!(result.is_empty());
    }

    #[test]
    fn test_multiple_fields() {
        // Keyword absent from title but present in description.
        let item = make_item("Greeting", Some("Hello World content"), None);
        let result = check_item_match(
            &item,
            &kws(&["World"]),
            "any",
            &fields(&["title", "description"]),
            false,
        );
        assert_eq!(result, kws(&["World"]));
    }

    // --- validate_params tests ---

    fn make_params(items: Vec<RssItem>, keywords: Vec<String>) -> FilterParams {
        FilterParams {
            items,
            keywords,
            match_mode: None,
            search_fields: None,
            case_sensitive: None,
        }
    }

    #[test]
    fn test_validate_ok() {
        let p = make_params(vec![make_item("title", None, None)], kws(&["kw"]));
        assert!(validate_params(&p).is_ok());
    }

    #[test]
    fn test_validate_too_many_items() {
        let items: Vec<RssItem> = (0..MAX_ITEMS + 1).map(|_| make_item("t", None, None)).collect();
        let p = make_params(items, kws(&["kw"]));
        assert!(validate_params(&p).is_err());
    }

    #[test]
    fn test_validate_too_many_keywords() {
        let keywords: Vec<String> = (0..MAX_KEYWORDS + 1).map(|i| format!("kw{}", i)).collect();
        let p = make_params(vec![make_item("t", None, None)], keywords);
        assert!(validate_params(&p).is_err());
    }

    #[test]
    fn test_validate_keyword_too_long() {
        let long_kw = "x".repeat(MAX_KEYWORD_LEN + 1);
        let p = make_params(vec![make_item("t", None, None)], vec![long_kw]);
        assert!(validate_params(&p).is_err());
    }

    #[test]
    fn test_validate_item_title_too_long() {
        let long_title = "x".repeat(MAX_ITEM_TEXT_LEN + 1);
        let p = make_params(vec![make_item(&long_title, None, None)], kws(&["kw"]));
        assert!(validate_params(&p).is_err());
    }

    #[test]
    fn test_validate_item_description_too_long() {
        let long_desc = "x".repeat(MAX_ITEM_TEXT_LEN + 1);
        let p = make_params(vec![make_item("t", Some(&long_desc), None)], kws(&["kw"]));
        assert!(validate_params(&p).is_err());
    }

    #[test]
    fn test_validate_item_content_too_long() {
        let long_content = "x".repeat(MAX_ITEM_TEXT_LEN + 1);
        let p = make_params(vec![make_item("t", None, Some(&long_content))], kws(&["kw"]));
        assert!(validate_params(&p).is_err());
    }
}
