//! A queue for batching decommits together.
//!
//! We don't immediately decommit a Wasm table/memory/stack/etc... eagerly, but
//! instead batch them up to be decommitted together. This module implements
//! that queuing and batching.
//!
//! Even when batching is "disabled" we still use this queue. Batching is
//! disabled by specifying a batch size of one, in which case, this queue will
//! immediately get flushed everytime we push onto it.

use super::PoolingInstanceAllocator;
use crate::vm::{MemoryAllocationIndex, MemoryImageSlot, Table, TableAllocationIndex};
use alloc::vec::Vec;
use core::mem;
use smallvec::SmallVec;

#[cfg(feature = "async")]
use wasmtime_fiber::FiberStack;

mod madvisev;

pub struct DecommitQueue {
    batch_size: usize,
    raw: SmallVec<[libc::iovec; 2]>,
    memories: SmallVec<[(MemoryAllocationIndex, MemoryImageSlot); 1]>,
    tables: SmallVec<[(TableAllocationIndex, Table); 1]>,
    #[cfg(feature = "async")]
    stacks: SmallVec<[FiberStack; 1]>,
    //
    // TODO: GC heaps are not well-integrated with the pooling allocator
    // yet. Once we better integrate them, we should start (optionally) zeroing
    // them, and batching that up here.
    //
    // #[cfg(feature = "gc")]
    // pub gc_heaps: SmallVec<[(GcHeapAllocationIndex, Box<dyn GcHeap>); 1]>,
}

unsafe impl Send for DecommitQueue {}
unsafe impl Sync for DecommitQueue {}

impl std::fmt::Debug for DecommitQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DecommitQueue")
            .field("batch_size", &self.batch_size)
            .field(
                "raw",
                &self
                    .raw
                    .iter()
                    .map(|v| (v.iov_base, v.iov_len))
                    .collect::<Vec<_>>(),
            )
            .field("memories", &"..")
            .field("tables", &"..")
            .field("stacks", &"..")
            .finish()
    }
}

impl DecommitQueue {
    /// Construct a new queue with the given batch size.
    pub fn new(batch_size: u32) -> Self {
        // Don't try to `madvise` more than the kernel can put in a single
        // iovec-array at the same time.
        let iov_max = u32::try_from(unsafe { libc::sysconf(libc::_SC_IOV_MAX) }).unwrap();
        let batch_size = core::cmp::min(batch_size, iov_max);
        let batch_size = usize::try_from(batch_size).unwrap();

        Self {
            batch_size,
            raw: Default::default(),
            memories: Default::default(),
            tables: Default::default(),
            #[cfg(feature = "async")]
            stacks: Default::default(),
        }
    }

    /// Take this queue and leave behind an empty queue with the same batch
    /// size.
    pub fn take(&mut self) -> Self {
        let new = Self::new(u32::try_from(self.batch_size).unwrap());
        mem::replace(self, new)
    }

    /// Enqueue a region of memory for decommit.
    ///
    /// It is the caller's responsibility to push the associated data via
    /// `self.push_{memory,table,stack}` as appropriate.
    ///
    /// Returns `true` if the queue is "full", i.e. has reached the target batch
    /// size, and should be flushed; returns `false` otherwise.
    ///
    /// Callers may continue to enqueue more memory regions even after this
    /// returns `true` if they must, but they should arrange for the queue to be
    /// flushed as soon as possible.
    ///
    /// # Safety
    ///
    /// The enqueued memory regions must be safe to decommit when `flush` is
    /// called (no other references, not in use, won't be otherwise unmapped,
    /// etc...).
    pub unsafe fn enqueue_raw(&mut self, ptr: *mut u8, len: usize) -> bool {
        self.raw.push(libc::iovec {
            iov_base: ptr.cast(),
            iov_len: len,
        });
        self.raw.len() >= self.batch_size
    }

    /// Push a memory into the queue.
    ///
    /// # Safety
    ///
    /// This memory should not be in use, and its decommit regions must have
    /// already been enqueued via `self.enqueue_raw`.
    pub unsafe fn push_memory(
        &mut self,
        allocation_index: MemoryAllocationIndex,
        image: MemoryImageSlot,
    ) {
        self.memories.push((allocation_index, image));
    }

    /// Push a table into the queue.
    ///
    /// # Safety
    ///
    /// This table should not be in use, and its decommit regions must have
    /// already been enqueued via `self.enqueue_raw`.
    pub unsafe fn push_table(&mut self, allocation_index: TableAllocationIndex, table: Table) {
        self.tables.push((allocation_index, table));
    }

    /// Push a stack into the queue.
    ///
    /// # Safety
    ///
    /// This stack should not be in use, and its decommit regions must have
    /// already been enqueued via `self.enqueue_raw`.
    #[cfg(feature = "async")]
    pub unsafe fn push_stack(&mut self, stack: FiberStack) {
        self.stacks.push(stack);
    }

    fn decommit_all_raw(&mut self) {
        for chunk in self.raw.chunks(1024) {
            unsafe { madvisev::madvisev(chunk, libc::MADV_DONTNEED) }
        }
        self.raw.clear();
    }

    /// Flush this queue
    pub fn flush(mut self, pool: &PoolingInstanceAllocator) {
        // First, do the raw decommit syscall(s).
        self.decommit_all_raw();

        // Second, restore the various entities to their associated pools' free
        // lists. This is safe, and they are ready for reuse, now that their
        // memory regions have been decommitted.
        for (allocation_index, image) in self.memories {
            unsafe {
                pool.memories.deallocate(allocation_index, image);
            }
        }
        for (allocation_index, table) in self.tables {
            unsafe {
                pool.tables.deallocate(allocation_index, table);
            }
        }
        #[cfg(feature = "async")]
        for stack in self.stacks {
            unsafe {
                pool.stacks.deallocate(stack);
            }
        }
    }
}
