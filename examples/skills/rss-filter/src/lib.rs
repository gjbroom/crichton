#![no_std]

extern crate alloc;

use alloc::{format, string::String, vec, vec::Vec};
use core::panic::PanicInfo;
use core::sync::atomic::{AtomicUsize, Ordering};
use serde::{Deserialize, Serialize};

const PAGE_SIZE: usize = 65536;
static HEAP_POS: AtomicUsize = AtomicUsize::new(0);

struct BumpAllocator;

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

            if HEAP_POS.compare_exchange(current, end, Ordering::Relaxed, Ordering::Relaxed).is_ok() {
                return aligned as *mut u8;
            }
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: core::alloc::Layout) {}
}

#[global_allocator]
static ALLOCATOR: BumpAllocator = BumpAllocator;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[link(wasm_import_module = "env")]
extern "C" {
    fn log(level: i32, ptr: *const u8, len: usize);
}

fn host_log(level: i32, msg: &str) {
    unsafe {
        log(level, msg.as_ptr(), msg.len());
    }
}

#[no_mangle]
pub extern "C" fn alloc(len: usize) -> *mut u8 {
    let layout = core::alloc::Layout::from_size_align(len, 1).unwrap();
    unsafe { alloc::alloc::alloc(layout) }
}

#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, len: usize) {
    let layout = core::alloc::Layout::from_size_align(len, 1).unwrap();
    unsafe { alloc::alloc::dealloc(ptr, layout) }
}

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
