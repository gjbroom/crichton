#![no_std]

extern crate alloc;

use core::panic::PanicInfo;
use core::sync::atomic::{AtomicUsize, Ordering};

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

#[no_mangle]
pub extern "C" fn filter_items() -> i32 {
    host_log(0, "filter_items: stub");
    0
}
