//! A global allocator that emits tracing events within entered [`Span`]s.
//!
//! See [`TracingAllocator`] for more information.
//!
//! [`Span`]: tracing::Span

use core::{
    alloc::{GlobalAlloc, Layout},
    cell::{RefCell, RefMut},
};

use std::panic::catch_unwind;

thread_local! {
    /// Flag controlling emission of allocator tracing events on this thread.
    static TRACE_ALLOCATOR: RefCell<bool> = RefCell::new(false);
}

/// A global allocator that emits tracing events within entered [`Span`]s.
///
/// [`Span`]: tracing::Span
///
/// This allocator emits [`TRACE`]-level events. See method documentation for
/// more information:
/// - [`TracingAllocator::alloc`]
/// - [`TracingAllocator::dealloc`]
/// - [`TracingAllocator::alloc_zeroed`]
/// - [`TracingAllocator::realloc`]
///
/// [`TRACE`]: crate::Level::TRACE
#[non_exhaustive]
#[derive(Debug, Copy, Clone)]
pub struct TracingAllocator<A> {
    allocator: A,
}

impl<A> TracingAllocator<A> {
    /// Constructs a tracing allocator.
    ///
    /// ## Usage
    /// ```
    /// use std::alloc::System;
    /// use tracing_allocations::TracingAllocator;
    ///
    /// #[global_allocator]
    /// static ALLOCATOR: TracingAllocator<System> = TracingAllocator::new(System);
    /// ```
    pub const fn new(allocator: A) -> Self {
        Self { allocator }
    }
}

unsafe impl<A> GlobalAlloc for TracingAllocator<A>
where
    A: GlobalAlloc,
{
    /// Allocate memory as described by the given `layout`.
    /// [Read more.][GlobalAlloc::alloc]
    ///
    /// Emits [`TRACE`]-level events with the following metadata:
    /// - **`name`**  
    ///   "alloc"
    /// - **`target`**  
    ///   "tracing::allocator"
    /// - **`addr`: [`usize`]**  
    ///   the address of the allocation
    /// - **`size`: [`usize`]**  
    ///   the size of the allocation
    ///
    /// [`TRACE`]: crate::Level::TRACE
    #[track_caller]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = self.allocator.alloc(layout);
        maybe_with_guard(|trace_allocations| {
            if *trace_allocations {
                // safety: global allocators must not unwind
                let _ = catch_unwind(|| {
                    crate::trace! {
                        addr = ptr as usize,
                        size = layout.size(),
                        "alloc",
                    }
                });
            }
        });
        ptr
    }

    /// Deallocate the block of memory at the given `ptr` pointer with the given
    /// `layout`.
    /// [Read more.][GlobalAlloc::dealloc]
    ///
    /// Emits [`TRACE`]-level events with the following metadata:
    /// - **`name`**  
    ///   "dealloc"
    /// - **`target`**  
    ///   "tracing::allocator"
    /// - **`addr`: [`usize`]**  
    ///   the address of the deallocation
    /// - **`size`: [`usize`]**  
    ///   the size of the deallocation
    ///
    /// [`TRACE`]: crate::Level::TRACE
    #[track_caller]
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.allocator.dealloc(ptr, layout);
        maybe_with_guard(|trace_allocations| {
            if *trace_allocations {
                // safety: global allocators must not unwind
                let _ = catch_unwind(|| {
                    crate::trace! {
                        addr = ptr as usize,
                        size = layout.size(),
                        "dealloc",
                    }
                });
            }
        });
    }

    /// Behaves like `alloc`, but also ensures that the contents are set to zero
    /// before being returned.
    /// [Read more.][GlobalAlloc::alloc_zeroed]
    ///
    /// Emits [`TRACE`]-level events with the following metadata:
    /// - **`name`**  
    ///   "alloc_zeroed"
    /// - **`target`**  
    ///   "tracing::allocator"
    /// - **`addr`: [`usize`]**  
    ///   the address of the allocation
    /// - **`size`: [`usize`]**  
    ///   the size of the allocation
    ///
    /// [`TRACE`]: crate::Level::TRACE
    #[track_caller]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let ptr = self.allocator.alloc_zeroed(layout);
        maybe_with_guard(|trace_allocations| {
            if *trace_allocations {
                // safety: global allocators must not unwind
                let _ = catch_unwind(|| {
                    crate::trace! {
                        addr = ptr as usize,
                        size = layout.size(),
                        "alloc_zeroed",
                    }
                });
            }
        });
        ptr
    }

    /// Shrink or grow a block of memory to the given `new_size`. The block is
    /// described by the given `old_ptr` pointer and `old_layout` layout.
    /// [Read more.][GlobalAlloc::realloc]
    ///
    /// Emits [`TRACE`]-level events with the following metadata:
    /// - **`name`**  
    ///   "realloc"
    /// - **`target`**  
    ///   "tracing::allocator"
    /// - **`old_addr`: [`usize`]**  
    ///   the address of the existing allocation
    /// - **`old_size`: [`usize`]**  
    ///   the size of the existing allocation
    /// - **`new_addr`: [`usize`]**  
    ///   the address of the new allocation
    /// - **`new_size`: [`usize`]**  
    ///   the size of the new allocation
    ///
    /// [`TRACE`]: crate::Level::TRACE
    #[track_caller]
    unsafe fn realloc(&self, old_ptr: *mut u8, old_layout: Layout, new_size: usize) -> *mut u8 {
        let new_ptr = self.allocator.realloc(old_ptr, old_layout, new_size);
        maybe_with_guard(|trace_allocations| {
            if *trace_allocations {
                // safety: global allocators must not unwind
                let _ = catch_unwind(|| {
                    crate::trace! {
                        old_addr = old_ptr as usize,
                        old_size = old_layout.size(),
                        new_addr = new_ptr as usize,
                        new_size = new_size,
                        "realloc",
                    }
                });
            }
        });
        new_ptr
    }
}

/// Possibly calls `f` with a unique, mutable reference to the [`TRACE_ALLOCATOR`]
/// flag.
///
/// This function will *not* call `f` if:
/// 1. The thread-local `TRACE_ALLOCATOR` flag cannot be acquired.
/// 2. The `TRACE_ALLOCATOR` flag cannot be mutably borrowed.
///
/// In practical terms, these conditions apply, respectively, if:
/// 1. `maybe_with_guard` is called within a destructor
/// 2. the [`TRACE_ALLOCATOR`] flag is already mutably borrowed
///
/// This second condition is useful to prevent recursive tracing of allocations.
fn maybe_with_guard<F>(f: F)
where
    F: for<'a> FnOnce(RefMut<'a, bool>),
{
    let _ = TRACE_ALLOCATOR.try_with(|guard| guard.try_borrow_mut().map(f));
}

/// Enable allocation tracing.
pub(crate) fn enable_tracing() {
    maybe_with_guard(|mut trace_allocations| {
        *trace_allocations = true;
    });
}

/// Disable allocation tracing.
///
/// **NOTE:** Allocation tracing should be disabled before the end of `main`.
pub(crate) fn disable_tracing() {
    maybe_with_guard(|mut trace_allocations| {
        *trace_allocations = false;
    });
}
