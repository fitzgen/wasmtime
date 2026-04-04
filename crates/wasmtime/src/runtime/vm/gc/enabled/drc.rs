//! The deferred reference-counting (DRC) collector.
//!
//! Warning: this ref-counting collector does not have a tracing cycle
//! collector, and therefore cannot collect cycles between GC objects!
//!
//! For host VM code, we use plain reference counting, where cloning increments
//! the reference count, and dropping decrements it. We can avoid many of the
//! on-stack increment/decrement operations that typically plague the
//! performance of reference counting via Rust's ownership and borrowing system.
//! Moving a `VMGcRef` avoids mutating its reference count, and borrowing it
//! either avoids the reference count increment or delays it until if/when the
//! `VMGcRef` is cloned.
//!
//! When passing a `VMGcRef` into compiled Wasm code, we don't want to do
//! reference count mutations for every compiled `local.{get,set}`, nor for
//! every function call. Therefore, we use a variation of **deferred reference
//! counting**, where we only mutate reference counts when storing `VMGcRef`s
//! somewhere that outlives the Wasm activation: into a global or
//! table. Simultaneously, we over-approximate the set of `VMGcRef`s that are
//! inside Wasm function activations. Periodically, we walk the stack at GC safe
//! points, and use stack map information to precisely identify the set of
//! `VMGcRef`s inside Wasm activations. Then we take the difference between this
//! precise set and our over-approximation, and decrement the reference count
//! for each of the `VMGcRef`s that are in our over-approximation but not in the
//! precise set. Finally, the over-approximation is reset to the precise set.
//!
//! An intrusive, singly-linked list in the object header implements the
//! over-approximated set of `VMGcRef`s referenced by Wasm activations. Calling
//! a Wasm function and passing it a `VMGcRef` inserts the `VMGcRef` into that
//! list if it is not already present, and the compiled Wasm function logically
//! "borrows" the `VMGcRef` from the list. Similarly, `global.get` and
//! `table.get` operations logically clone the gotten `VMGcRef` into that list
//! and then "borrow" the reference out of the list.
//!
//! When a `VMGcRef` is returned to host code from a Wasm function, the host
//! increments the reference count (because the reference is logically
//! "borrowed" from the list and the reference count from
//! the table will be dropped at the next GC).
//!
//! The precise set of stack roots is implemented with a mark bit in the object
//! header. See the `trace` and `sweep` methods for more details.
//!
//! For more general information on deferred reference counting, see *An
//! Examination of Deferred Reference Counting and Cycle Detection* by Quinane:
//! <https://openresearch-repository.anu.edu.au/bitstream/1885/42030/2/hon-thesis.pdf>

use super::VMArrayRef;
use super::free_list::FreeList;
use crate::hash_set::HashSet;
use crate::runtime::vm::{
    ExternRefHostDataId, ExternRefHostDataTable, GarbageCollection, GcHeap, GcHeapObject,
    GcProgress, GcRootsIter, GcRuntime, TypedGcRef, VMExternRef, VMGcHeader, VMGcRef,
};
use crate::vm::VMMemoryDefinition;
use crate::{Engine, EngineWeak, prelude::*};
use core::sync::atomic::AtomicUsize;
use core::{
    alloc::Layout,
    any::Any,
    mem,
    num::NonZeroU32,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};
use wasmtime_environ::drc::{ARRAY_LENGTH_OFFSET, DrcTypeLayouts};
use wasmtime_environ::{
    GcArrayLayout, GcLayout, GcStructLayout, GcTypeLayouts, POISON, VMGcKind, VMSharedTypeIndex,
    gc_assert,
};

#[expect(clippy::cast_possible_truncation, reason = "known to not overflow")]
const GC_REF_ARRAY_ELEMS_OFFSET: u32 = ARRAY_LENGTH_OFFSET + (mem::size_of::<u32>() as u32);

/// The deferred reference-counting (DRC) collector.
///
/// This reference-counting collector does not have a cycle collector, and so it
/// will not be able to reclaim garbage cycles.
///
/// This is not a moving collector; it doesn't have a nursery or do any
/// compaction.
#[derive(Default)]
pub struct DrcCollector {
    layouts: DrcTypeLayouts,
}

unsafe impl GcRuntime for DrcCollector {
    fn layouts(&self) -> &dyn GcTypeLayouts {
        &self.layouts
    }

    fn new_gc_heap(&self, engine: &Engine) -> Result<Box<dyn GcHeap>> {
        let heap = DrcHeap::new(engine)?;
        Ok(Box::new(heap) as _)
    }
}

/// How to trace a GC object.
enum TraceInfo {
    /// How to trace an array.
    Array {
        /// Whether this array type's elements are GC references, and need
        /// tracing.
        gc_ref_elems: bool,
    },

    /// How to trace a struct.
    Struct {
        /// The offsets of each GC reference field that needs tracing in
        /// instances of this struct type.
        gc_ref_offsets: Box<[u32]>,
    },
}

/// A deferred reference-counting (DRC) heap.
pub(crate) struct DrcHeap {
    engine: EngineWeak,

    /// For every type that we have allocated in this heap, how do we trace it?
    /// Indexed by VMSharedTypeIndex::bits() for O(1) lookup without hashing.
    trace_infos: Vec<Option<TraceInfo>>,

    /// Count of how many no-gc scopes we are currently within.
    no_gc_count: u64,

    /// The head of the over-approximated-stack-roots list.
    ///
    /// Note that this is exposed directly to compiled Wasm code through the
    /// vmctx, so must not move.
    over_approximated_stack_roots: Option<VMGcRef>,

    /// The storage for the GC heap itself.
    memory: Option<crate::vm::Memory>,

    /// The cached `VMMemoryDefinition` for `self.memory` so that we don't have
    /// to make indirect calls through a `dyn RuntimeLinearMemory` object.
    ///
    /// Must be updated and kept in sync with `self.memory`, cleared when the
    /// memory is taken and updated when the memory is replaced.
    vmmemory: Option<VMMemoryDefinition>,

    /// A free list describing which ranges of the heap are available for use.
    free_list: Option<FreeList>,

    /// An explicit stack to avoid recursion when deallocating one object needs
    /// to dec-ref another object, which can then be deallocated and dec-refs
    /// yet another object, etc...
    ///
    /// We store this stack here to reuse the storage and avoid repeated
    /// allocations.
    ///
    /// Note that the `Option` is perhaps technically unnecessary (we could
    /// remove the `Option` and, when we take the stack out of `self`, leave
    /// behind an empty vec instead of `None`) but we keep it because it will
    /// help us catch unexpected re-entry, similar to how a `RefCell` would.
    dec_ref_stack: Vec<VMGcRef>,

    /// Cached type index for which ensure_trace_info was last called.
    /// Avoids repeated lookups when allocating the same type repeatedly.
    last_ensured_trace_info_ty: u32,
}

impl DrcHeap {
    /// Construct a new, default DRC heap.
    fn new(engine: &Engine) -> Result<Self> {
        log::trace!("allocating new DRC heap");
        Ok(Self {
            engine: engine.weak(),
            trace_infos: Vec::new(),
            no_gc_count: 0,
            over_approximated_stack_roots: None,
            memory: None,
            vmmemory: None,
            free_list: None,
            dec_ref_stack: Vec::with_capacity(1),
            last_ensured_trace_info_ty: u32::MAX,
        })
    }

    fn engine(&self) -> Engine {
        self.engine.upgrade().unwrap()
    }

    fn dealloc(&mut self, gc_ref: VMGcRef) {
        let drc_ref = drc_ref(&gc_ref);
        let size = self.index(drc_ref).object_size;
        let alloc_size = FreeList::aligned_size(size);
        // SAFETY: gc_ref is a non-i31 heap ref, always has a valid index.
        let index = unsafe { gc_ref.as_heap_index().unwrap_unchecked() };

        // Poison the freed memory so that any stale access is detectable.
        if cfg!(gc_zeal) {
            let idx = usize::try_from(index.get()).unwrap();
            self.heap_slice_mut()[idx..][..usize::try_from(alloc_size).unwrap()].fill(POISON);
        }

        // SAFETY: free_list is always Some after heap initialization.
        unsafe { self.free_list.as_mut().unwrap_unchecked() }.dealloc_fast(index, alloc_size);
    }

    /// Increment the ref count for the associated object.
    fn inc_ref(&mut self, gc_ref: &VMGcRef) {
        if gc_ref.is_i31() {
            return;
        }

        let drc_ref = drc_ref(gc_ref);
        let header = self.index_mut(&drc_ref);
        debug_assert_ne!(
            header.ref_count, 0,
            "{:#p} is supposedly live; should have nonzero ref count",
            *gc_ref
        );
        header.ref_count += 1;
    }

    /// Decrement the ref count for the associated object.
    ///
    /// Returns `true` if the ref count reached zero and the object should be
    /// deallocated.
    fn dec_ref(&mut self, gc_ref: &VMGcRef) -> bool {
        if gc_ref.is_i31() {
            return false;
        }

        let drc_ref = drc_ref(gc_ref);
        let header = self.index_mut(drc_ref);
        debug_assert_ne!(
            header.ref_count, 0,
            "{:#p} is supposedly live; should have nonzero ref count",
            *gc_ref
        );
        header.ref_count -= 1;
        header.ref_count == 0
    }

    /// Decrement the ref count for the associated object.
    ///
    /// If the ref count reached zero, then deallocate the object and remove its
    /// associated entry from the `host_data_table` if necessary.
    ///
    /// This uses an explicit stack, rather than recursion, for the scenario
    /// where dropping one object means that the ref count for another object
    /// that it referenced reaches zero.
    #[inline(always)]
    fn dec_ref_and_maybe_dealloc(
        &mut self,
        host_data_table: &mut ExternRefHostDataTable,
        gc_ref: &VMGcRef,
    ) {
        // Fast path: if this is a non-i31 ref and the ref count stays above
        // zero, we can just decrement and return without touching the
        // dec_ref_stack at all. This avoids the take/put and stack push/pop
        // overhead for the common case where ref_count goes 2→1.
        if gc_ref.is_i31() {
            return;
        }

        let vmmemory = self.vmmemory();
        let heap_base = vmmemory.base.as_ptr();
        let _heap_len = vmmemory.current_length();
        let start = unsafe { gc_ref.as_heap_index().unwrap_unchecked() }.get() as usize;
        debug_assert!(start + core::mem::size_of::<VMDrcHeader>() <= _heap_len);
        let drc_header =
            unsafe { &mut *(heap_base.add(start) as *mut VMDrcHeader) };
        debug_assert_ne!(drc_header.ref_count, 0);
        drc_header.ref_count -= 1;
        if drc_header.ref_count != 0 {
            return;
        }

        // Ref count reached zero. Set it back to 1 so the cascade loop can
        // uniformly decrement all items without a special first-iteration flag.
        drc_header.ref_count = 1;
        self.dec_ref_cascade(host_data_table, gc_ref, heap_base, _heap_len);
    }

    fn dec_ref_cascade(
        &mut self,
        host_data_table: &mut ExternRefHostDataTable,
        gc_ref: &VMGcRef,
        heap_base: *mut u8,
        _heap_len: usize,
    ) {
        // Use a fixed-size array on the function stack as a fast stack for
        // non-tail children. This avoids Vec overhead (capacity checks, pointer
        // indirection). For binary-trees (depth ~21, 2 children per node with
        // tail optimization), the stack never exceeds ~21 entries. For rare
        // overflow (e.g., large arrays of gc_refs), fall back to self.dec_ref_stack.
        const FAST_STACK_CAP: usize = 64;
        let mut fast_stack = [0u32; FAST_STACK_CAP];
        let mut fast_len: usize = 0;

        // Raw pointer to self.dec_ref_stack for overflow. SAFETY: dec_ref_stack
        // is a separate field from free_list/trace_infos/heap, so no aliasing.
        let overflow_ptr: *mut Vec<VMGcRef> = &mut self.dec_ref_stack;
        debug_assert!(unsafe { &*overflow_ptr }.is_empty());

        // Cache pointer to free_list so we don't reload through self's Option
        // on every iteration.
        // SAFETY: free_list is always Some after heap initialization.
        let free_list_ptr: *mut FreeList =
            unsafe { self.free_list.as_mut().unwrap_unchecked() as *mut FreeList };

        // Inline helper: push a non-tail child onto the stack.
        macro_rules! stack_push {
            ($child:expr) => {
                if fast_len < FAST_STACK_CAP {
                    unsafe {
                        *fast_stack.get_unchecked_mut(fast_len) =
                            $child.as_raw_non_zero_u32().get();
                    }
                    fast_len += 1;
                } else {
                    unsafe { &mut *overflow_ptr }.push($child);
                }
            };
        }

        // Process gc_ref directly. Enter the tail-call processing loop
        // with gc_ref as the first item. Its ref_count was set back to 1 by the
        // caller so we can uniformly decrement here.
        let mut current = gc_ref.unchecked_copy();
        'outer: loop {
            // Tail-call loop: process the last child of a struct inline
            // instead of pushing it to the stack, saving one push+pop per node.
            loop {
                if current.is_i31() {
                    break;
                }

                // SAFETY: current is a non-i31 gc_ref that was allocated by us,
                // so it always has a valid heap index.
                let index = unsafe { current.as_heap_index().unwrap_unchecked() };
                let start = index.get() as usize;

                // Read the DRC header directly via the cached heap base pointer,
                // skipping the two bounds checks in index_mut.
                //
                // SAFETY: The gc_ref was allocated with at least VMDrcHeader
                // size, and the heap base/len don't change during dealloc.
                // The heap memory is disjoint from self's struct fields.
                debug_assert!(start + core::mem::size_of::<VMDrcHeader>() <= _heap_len);
                let drc_header =
                    unsafe { &mut *(heap_base.add(start) as *mut VMDrcHeader) };

                debug_assert_ne!(
                    drc_header.ref_count, 0,
                    "{:#p} is supposedly live; should have nonzero ref count",
                    current
                );
                drc_header.ref_count -= 1;
                if drc_header.ref_count != 0 {
                    break;
                }

                // Ref count reached zero. Extract type and size from the header
                // we already read (avoiding re-reading from heap).
                let ty = drc_header.header.ty();
                let object_size = drc_header.object_size;

                // Trace: find children and collect the last valid child for
                // tail processing. All other children are pushed to the stack.
                let mut tail_child = None;
                if let Some(ty) = ty {
                    // SAFETY: ty was written by ensure_trace_info which
                    // guarantees the index exists and is Some.
                    let trace_info = unsafe {
                        self.trace_infos
                            .get_unchecked(ty.bits() as usize)
                            .as_ref()
                            .unwrap_unchecked()
                    };
                    match trace_info {
                        TraceInfo::Struct { gc_ref_offsets } => {
                            // Read gc_ref fields using the cached heap base
                            // pointer and unchecked pointer access.
                            for offset in gc_ref_offsets {
                                let off = *offset as usize;
                                // SAFETY: offset is within the allocated object
                                // which is within the heap bounds.
                                let raw = unsafe {
                                    core::ptr::read_unaligned(
                                        heap_base.add(start + off) as *const u32,
                                    )
                                };
                                if let Some(child) = VMGcRef::from_raw_u32(raw)
                                    && !child.is_i31()
                                {
                                    debug_assert!({
                                        let header = self.header(&child);
                                        let kind = header.kind().as_u32();
                                        VMGcKind::try_from_u32(kind).is_some()
                                    });
                                    // Keep replacing tail_child; previous one
                                    // goes to the stack.
                                    if let Some(prev) = tail_child.replace(child) {
                                        stack_push!(prev);
                                    }
                                }
                            }
                        }
                        TraceInfo::Array { gc_ref_elems } => {
                            if *gc_ref_elems {
                                let data = self.gc_object_data(&current);
                                let len = self.array_len(current.as_arrayref_unchecked());
                                for i in 0..len {
                                    let elem_offset = GC_REF_ARRAY_ELEMS_OFFSET
                                        + i * u32::try_from(mem::size_of::<u32>()).unwrap();
                                    let raw = data.read_u32(elem_offset);
                                    if let Some(child) = VMGcRef::from_raw_u32(raw)
                                        && !child.is_i31()
                                    {
                                        debug_assert!({
                                            let header = self.header(&child);
                                            let kind = header.kind().as_u32();
                                            VMGcKind::try_from_u32(kind).is_some()
                                        });
                                        if let Some(prev) = tail_child.replace(child) {
                                            stack_push!(prev);
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    debug_assert!(drc_header.header.kind().matches(VMGcKind::ExternRef));

                    // Handle externref host data. Only externrefs have host data,
                    // and ty is None only for externrefs, so we skip this for
                    // struct/array objects entirely.
                    if let Some(externref) = current.as_typed::<VMDrcExternRef>(self) {
                        let host_data_id = self.index(externref).host_data;
                        host_data_table.dealloc(host_data_id);
                    }
                }

                // Deallocate using the object_size we already read.
                let alloc_size = FreeList::aligned_size(object_size);

                if cfg!(gc_zeal) {
                    let idx = usize::try_from(index.get()).unwrap();
                    self.heap_slice_mut()[idx..][..usize::try_from(alloc_size).unwrap()]
                        .fill(POISON);
                }

                // SAFETY: free_list_ptr is valid for the lifetime of this function.
                unsafe { &mut *free_list_ptr }
                    .dealloc_fast(index, alloc_size);

                // Process the last child inline instead of via stack.
                match tail_child {
                    Some(child) => current = child,
                    None => break,
                }
            }

            // Pop the next item from the stack, or finish.
            // Check overflow Vec first, then fast stack.
            if let Some(next) = unsafe { &mut *overflow_ptr }.pop() {
                current = next;
            } else if fast_len > 0 {
                fast_len -= 1;
                let raw = unsafe { *fast_stack.get_unchecked(fast_len) };
                // SAFETY: we only push non-zero values from VMGcRef.
                current = unsafe {
                    VMGcRef::from_raw_non_zero_u32(NonZeroU32::new_unchecked(raw))
                };
            } else {
                break 'outer;
            }
        }
    }

    /// Ensure that we have tracing information for the given type.
    #[inline]
    fn ensure_trace_info(&mut self, ty: VMSharedTypeIndex) {
        if self.last_ensured_trace_info_ty == ty.bits() {
            return;
        }
        self.ensure_trace_info_slow(ty);
    }

    #[inline(never)]
    fn ensure_trace_info_slow(&mut self, ty: VMSharedTypeIndex) {
        let idx = ty.bits() as usize;
        if idx < self.trace_infos.len() && self.trace_infos[idx].is_some() {
            self.last_ensured_trace_info_ty = ty.bits();
            return;
        }

        self.insert_new_trace_info(ty);
        self.last_ensured_trace_info_ty = ty.bits();
    }

    fn insert_new_trace_info(&mut self, ty: VMSharedTypeIndex) {
        let idx = ty.bits() as usize;
        debug_assert!(idx >= self.trace_infos.len() || self.trace_infos[idx].is_none());

        let engine = self.engine();
        let gc_layout = engine
            .signatures()
            .layout(ty)
            .unwrap_or_else(|| panic!("should have a GC layout for {ty:?}"));

        let info = match gc_layout {
            GcLayout::Array(l) => {
                if l.elems_are_gc_refs {
                    debug_assert_eq!(l.elem_offset(0), GC_REF_ARRAY_ELEMS_OFFSET,);
                }
                TraceInfo::Array {
                    gc_ref_elems: l.elems_are_gc_refs,
                }
            }
            GcLayout::Struct(l) => TraceInfo::Struct {
                gc_ref_offsets: l
                    .fields
                    .iter()
                    .filter_map(|f| if f.is_gc_ref { Some(f.offset) } else { None })
                    .collect(),
            },
        };

        if idx >= self.trace_infos.len() {
            self.trace_infos.resize_with(idx + 1, || None);
        }
        let old_entry = self.trace_infos[idx].replace(info);
        debug_assert!(old_entry.is_none());
    }

    /// Enumerate all of the given `VMGcRef`'s outgoing edges.
    fn trace_gc_ref(&self, gc_ref: &VMGcRef, stack: &mut Vec<VMGcRef>) {
        debug_assert!(!gc_ref.is_i31());

        let header = self.header(gc_ref);
        let Some(ty) = header.ty() else {
            debug_assert!(header.kind().matches(VMGcKind::ExternRef));
            return;
        };

        match self
            .trace_infos[ty.bits() as usize]
            .as_ref()
            .expect("should have inserted trace info for every GC type allocated in this heap")
        {
            TraceInfo::Struct { gc_ref_offsets } => {
                stack.reserve(gc_ref_offsets.len());
                let data = self.gc_object_data(gc_ref);
                for offset in gc_ref_offsets {
                    let raw = data.read_u32(*offset);
                    if let Some(gc_ref) = VMGcRef::from_raw_u32(raw)
                        && !gc_ref.is_i31()
                    {
                        debug_assert!(
                            {
                                let header = self.header(&gc_ref);
                                let kind = header.kind().as_u32();
                                VMGcKind::try_from_u32(kind).is_some()
                            },
                            "trace_gc_ref: struct field at offset {offset} references object \
                             with invalid `VMGcKind`",
                        );

                        stack.push(gc_ref);
                    }
                }
            }

            TraceInfo::Array { gc_ref_elems } => {
                if !*gc_ref_elems {
                    return;
                }

                let data = self.gc_object_data(gc_ref);
                let len = self.array_len(gc_ref.as_arrayref_unchecked());
                stack.reserve(usize::try_from(len).unwrap());
                for i in 0..len {
                    let elem_offset = GC_REF_ARRAY_ELEMS_OFFSET
                        + i * u32::try_from(mem::size_of::<u32>()).unwrap();
                    let raw = data.read_u32(elem_offset);
                    if let Some(gc_ref) = VMGcRef::from_raw_u32(raw)
                        && !gc_ref.is_i31()
                    {
                        debug_assert!(
                            {
                                let header = self.header(&gc_ref);
                                let kind = header.kind().as_u32();
                                VMGcKind::try_from_u32(kind).is_some()
                            },
                            "trace_gc_ref: array element at index {i} references object \
                             with invalid `VMGcKind`",
                        );

                        stack.push(gc_ref);
                    }
                }
            }
        }
    }

    /// Iterate over the over-approximated-stack-roots list.
    fn iter_over_approximated_stack_roots(&self) -> impl Iterator<Item = VMGcRef> + '_ {
        let mut link = self.over_approximated_stack_roots
            .as_ref()
            .map(|r| r.unchecked_copy());

        core::iter::from_fn(move || {
            let r = link.as_ref()?.unchecked_copy();
            link = self.index(drc_ref(&r)).next_over_approximated_stack_root();
            Some(r)
        })
    }

    /// Assert the integrity of the over-approximated stack roots list.
    fn assert_over_approximated_stack_roots_integrity(&self) {
        if !cfg!(gc_zeal) {
            return;
        }

        let mut visited = HashSet::new();
        for gc_ref in self.iter_over_approximated_stack_roots() {
            let idx = gc_ref.as_heap_index().unwrap().get();

            // Each entry must have a valid `VMGcKind`.
            let header = self.header(&gc_ref);
            let kind = header.kind().as_u32();
            assert!(
                VMGcKind::try_from_u32(kind).is_some(),
                "over-approx list: entry at heap index {idx} has invalid VMGcKind {kind:#034b}",
            );

            // Each entry must have its in-list bit set.
            let drc_header = self.index(drc_ref(&gc_ref));
            assert!(
                drc_header.is_in_over_approximated_stack_roots(),
                "over-approx list: entry at heap index {idx} does not have in-list bit set",
            );

            // Each entry must have a nonzero ref count.
            assert_ne!(
                drc_header.ref_count, 0,
                "over-approx list: entry at heap index {idx} has zero ref count",
            );

            // No cycles or duplicates.
            assert!(
                visited.insert(idx),
                "over-approx list: cycle or duplicate detected at heap index {idx}",
            );
        }
    }

    /// Assert that every free block in the free list is filled with the poison
    /// pattern.
    fn assert_free_blocks_are_poisoned(&self) {
        if !cfg!(gc_zeal) {
            return;
        }

        let free_list = self.free_list.as_ref().unwrap();
        for (index, len) in free_list.iter_free_blocks() {
            let start = usize::try_from(index).unwrap();
            let size = usize::try_from(len).unwrap();
            let slice = &self.heap_slice()[start..][..size];
            assert!(
                slice.iter().all(|&b| b == POISON),
                "free block at heap index {start} (size {size}) is not fully poisoned",
            );
        }
    }

    fn trace(&mut self, roots: &mut GcRootsIter<'_>) {
        // The `over_approx_set` is used for `debug_assert!`s checking that
        // every reference we read out from the stack via stack maps is actually
        // in the table. If that weren't true, than either we forgot to insert a
        // reference in the table when passing it into Wasm (a bug) or we are
        // reading invalid references from the stack (another bug).
        let mut over_approx_set: DebugOnly<HashSet<_>> = Default::default();
        if cfg!(debug_assertions) {
            over_approx_set.extend(self.iter_over_approximated_stack_roots());
        }

        for root in roots {
            if !root.is_on_wasm_stack() {
                // We only trace on-Wasm-stack GC roots. These are the
                // GC references that we do deferred ref counting for
                // and that get inserted into our activations
                // table. Other GC roots are managed purely with naive
                // ref counting.
                continue;
            }

            let gc_ref = root.get();

            if gc_ref.is_i31() {
                continue;
            }

            debug_assert!(
                over_approx_set.contains(&gc_ref),
                "every on-stack gc ref inside a Wasm frame should \
                 have be in our over-approximated stack roots set, \
                 but {gc_ref:#p} is not in the set",
            );
            debug_assert!(
                self.index(drc_ref(&gc_ref))
                    .is_in_over_approximated_stack_roots(),
                "every on-stack gc ref inside a Wasm frame should have \
                 its in-the-over-approximated-stack-roots-list bit set",
            );
            debug_assert_ne!(
                self.index_mut(drc_ref(&gc_ref)).ref_count,
                0,
                "{gc_ref:#p} is on the Wasm stack and therefore should be held \
                 alive by the over-approximated-stack-roots set; should have \
                 nonzero ref count",
            );

            self.index_mut(drc_ref(&gc_ref)).set_marked();
        }
    }

    #[inline(never)]
    #[cold]
    fn log_gc_ref_set(prefix: &str, items: impl Iterator<Item = VMGcRef>) {
        assert!(log::log_enabled!(log::Level::Trace));
        let mut set = "{".to_string();
        let mut any = false;
        for gc_ref in items {
            any = true;
            set += &format!("\n  {gc_ref:#p},");
        }
        if any {
            set.push('\n');
        }
        set.push('}');
        log::trace!("{prefix}: {set}");
    }

    /// Sweep the bump allocation table after we've discovered our precise stack
    /// roots.
    fn sweep(&mut self, host_data_table: &mut ExternRefHostDataTable) {
        // Logically, we are taking the difference between
        // over-approximated-stack-roots set and the precise-stack-roots set,
        // decrementing the ref count for each object in that difference
        // (because they are no longer live on the stack), and then resetting
        // the over-approximated-stack-roots set to the precise set. In our
        // actual implementation, the over-approximated-stack-roots set is
        // implemented as an intrusive, singly-linked list in the object
        // headers, and the precise-stack-roots set is implemented via the mark
        // bits in the object headers. Therefore, we walk the
        // over-approximated-stack-roots list, checking whether each object has
        // its mark bit set.
        //
        // * If the mark bit is set, then it is in the precise-stack-roots set
        //   and is still on the stack, so we keep it in the
        //   over-approximated-stack-roots list and do not modify its ref count.
        //
        // * If the mark bit is not set, then it is not in the
        //   precise-stack-roots set and is no longer on the stack, so we remove
        //   it from the over-approximated-stack-roots set and decrement its ref
        //   count.
        //
        // We also clear the mark bits as we do this traversal.
        //
        // Finally, note that decrementing ref counts may run `Drop`
        // implementations, which may run arbitrary user code. However, because
        // of our `&mut` borrow on this heap (which ultimately comes from a
        // `&mut Store`) we're guaranteed that nothing will reentrantly touch
        // this heap or run Wasm code in this store.

        // The `VMGcRef` of the previous object in the
        // over-approximated-stack-roots list, if any.
        let mut prev = None;

        // The `VMGcRef` of the next object in the over-approximated-stack-roots
        // list, if any.
        let mut next = self.over_approximated_stack_roots
            .as_ref()
            .map(|r| r.unchecked_copy());

        // Cache the heap base pointer outside the loop. The heap doesn't grow
        // or move during sweep/dec_ref processing.
        let vmmemory = self.vmmemory();
        let heap_base = vmmemory.base.as_ptr();
        let _heap_len = vmmemory.current_length();

        while let Some(gc_ref) = next {
            // Use direct pointer access to skip index_mut bounds checks.
            // SAFETY: gc_ref points to a valid VMDrcHeader within the heap,
            // and the heap memory is disjoint from self's struct fields.
            let start = unsafe { gc_ref.as_heap_index().unwrap_unchecked() }.get() as usize;
            let header =
                unsafe { &mut *(heap_base.add(start) as *mut VMDrcHeader) };
            debug_assert!(header.is_in_over_approximated_stack_roots());

            if header.clear_marked() {
                next = header.next_over_approximated_stack_root();
                prev = Some(gc_ref);
                continue;
            }

            // This GC ref was not marked, so remove it from the
            // over-approximated-stack-roots list and decrement its ref count.
            let next_oasr = header.next_over_approximated_stack_root();
            next = next_oasr.as_ref().map(|r| r.unchecked_copy());
            header.set_in_over_approximated_stack_roots_bit(false);
            match &prev {
                None => self.over_approximated_stack_roots = next_oasr,
                Some(prev) => {
                    let prev_start = unsafe { prev.as_heap_index().unwrap_unchecked() }.get() as usize;
                    let prev_header =
                        unsafe { &mut *(heap_base.add(prev_start) as *mut VMDrcHeader) };
                    prev_header.set_next_over_approximated_stack_root(next_oasr);
                }
            }
            // Inline dec_ref: we already have the header, skip redundant
            // i31 check, vmmemory load, and heap index conversion.
            debug_assert_ne!(header.ref_count, 0);
            header.ref_count -= 1;
            if header.ref_count == 0 {
                header.ref_count = 1;
                self.dec_ref_cascade(host_data_table, &gc_ref, heap_base, _heap_len);
            }
        }
    }
}

/// Convert the given GC reference as a typed GC reference pointing to a
/// `VMDrcHeader`.
fn drc_ref(gc_ref: &VMGcRef) -> &TypedGcRef<VMDrcHeader> {
    debug_assert!(!gc_ref.is_i31());
    gc_ref.as_typed_unchecked()
}

/// Convert a generic `externref` to a typed reference to our concrete
/// `externref` type.
fn externref_to_drc(externref: &VMExternRef) -> &TypedGcRef<VMDrcExternRef> {
    let gc_ref = externref.as_gc_ref();
    debug_assert!(!gc_ref.is_i31());
    gc_ref.as_typed_unchecked()
}

/// The common header for all objects in the DRC collector.
///
/// This adds a ref count on top collector-agnostic `VMGcHeader`.
///
/// This is accessed by JIT code.
#[repr(C)]
struct VMDrcHeader {
    header: VMGcHeader,
    ref_count: u64,
    next_over_approximated_stack_root: Option<VMGcRef>,
    object_size: u32,
}

unsafe impl GcHeapObject for VMDrcHeader {
    #[inline]
    fn is(_header: &VMGcHeader) -> bool {
        // All DRC objects have a DRC header.
        true
    }
}

impl VMDrcHeader {
    /// The size of this header's object.
    #[inline]
    fn object_size(&self) -> usize {
        usize::try_from(self.object_size).unwrap()
    }

    /// Is this object in the over-approximated stack roots list?
    #[inline]
    fn is_in_over_approximated_stack_roots(&self) -> bool {
        self.header.reserved_u26() & wasmtime_environ::drc::HEADER_IN_OVER_APPROX_LIST_BIT != 0
    }

    /// Set whether this object is in the over-approximated stack roots list.
    #[inline]
    fn set_in_over_approximated_stack_roots_bit(&mut self, bit: bool) {
        let reserved = self.header.reserved_u26();
        let new_reserved = if bit {
            reserved | wasmtime_environ::drc::HEADER_IN_OVER_APPROX_LIST_BIT
        } else {
            reserved & !wasmtime_environ::drc::HEADER_IN_OVER_APPROX_LIST_BIT
        };
        self.header.set_reserved_u26(new_reserved);
    }

    /// Get the next object after this one in the over-approximated-stack-roots
    /// list, if any.
    #[inline]
    fn next_over_approximated_stack_root(&self) -> Option<VMGcRef> {
        debug_assert!(self.is_in_over_approximated_stack_roots());
        self.next_over_approximated_stack_root
            .as_ref()
            .map(|r| r.unchecked_copy())
    }

    /// Set the next object after this one in the over-approximated-stack-roots
    /// list.
    #[inline]
    fn set_next_over_approximated_stack_root(&mut self, next: Option<VMGcRef>) {
        debug_assert!(self.is_in_over_approximated_stack_roots());
        self.next_over_approximated_stack_root = next;
    }

    /// Is this object marked?
    #[inline]
    fn is_marked(&self) -> bool {
        self.header.reserved_u26() & wasmtime_environ::drc::HEADER_MARK_BIT != 0
    }

    /// Mark this object.
    ///
    /// Returns `true` if this object was newly marked (i.e. `is_marked()` would
    /// have returned `false` before this call was made).
    #[inline]
    fn set_marked(&mut self) {
        let reserved = self.header.reserved_u26();
        self.header
            .set_reserved_u26(reserved | wasmtime_environ::drc::HEADER_MARK_BIT);
    }

    /// Clear the mark bit for this object.
    ///
    /// Returns `true` if this object was marked before the mark bit was
    /// cleared.
    #[inline]
    fn clear_marked(&mut self) -> bool {
        if self.is_marked() {
            let reserved = self.header.reserved_u26();
            self.header
                .set_reserved_u26(reserved & !wasmtime_environ::drc::HEADER_MARK_BIT);
            debug_assert!(!self.is_marked());
            true
        } else {
            false
        }
    }
}

/// The common header for all arrays in the DRC collector.
#[repr(C)]
struct VMDrcArrayHeader {
    header: VMDrcHeader,
    length: u32,
}

unsafe impl GcHeapObject for VMDrcArrayHeader {
    #[inline]
    fn is(header: &VMGcHeader) -> bool {
        header.kind() == VMGcKind::ArrayRef
    }
}

/// The representation of an `externref` in the DRC collector.
#[repr(C)]
struct VMDrcExternRef {
    header: VMDrcHeader,
    host_data: ExternRefHostDataId,
}

unsafe impl GcHeapObject for VMDrcExternRef {
    #[inline]
    fn is(header: &VMGcHeader) -> bool {
        header.kind() == VMGcKind::ExternRef
    }
}

unsafe impl GcHeap for DrcHeap {
    fn is_attached(&self) -> bool {
        debug_assert_eq!(self.memory.is_some(), self.free_list.is_some());
        debug_assert_eq!(self.memory.is_some(), self.vmmemory.is_some());
        self.memory.is_some()
    }

    fn attach(&mut self, memory: crate::vm::Memory) {
        assert!(!self.is_attached());
        assert!(!memory.is_shared_memory());
        debug_assert!(self.over_approximated_stack_roots.is_none());
        let len = memory.vmmemory().current_length();
        self.free_list = Some(FreeList::new(len));
        self.vmmemory = Some(memory.vmmemory());
        self.memory = Some(memory);

        // Poison the entire heap so any access to uninitialized memory is
        // detectable.
        if cfg!(gc_zeal) {
            self.heap_slice_mut().fill(POISON);
        }
    }

    fn detach(&mut self) -> crate::vm::Memory {
        assert!(self.is_attached());

        let DrcHeap {
            engine: _,
            no_gc_count,
            over_approximated_stack_roots,
            free_list,
            dec_ref_stack,
            memory,
            vmmemory,

            // NB: we will only ever be reused with the same engine, so no need
            // to clear out our tracing info just to fill it back in with the
            // same exact stuff.
            trace_infos: _,
            last_ensured_trace_info_ty: _,
        } = self;

        *no_gc_count = 0;
        *over_approximated_stack_roots = None;
        *free_list = None;
        *vmmemory = None;
        debug_assert!(dec_ref_stack.is_empty());

        memory.take().unwrap()
    }

    fn as_any(&self) -> &dyn Any {
        self as _
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self as _
    }

    fn enter_no_gc_scope(&mut self) {
        self.no_gc_count += 1;
    }

    fn exit_no_gc_scope(&mut self) {
        self.no_gc_count -= 1;
    }

    fn clone_gc_ref(&mut self, gc_ref: &VMGcRef) -> VMGcRef {
        self.inc_ref(gc_ref);
        gc_ref.unchecked_copy()
    }

    fn write_gc_ref(
        &mut self,
        host_data_table: &mut ExternRefHostDataTable,
        destination: &mut Option<VMGcRef>,
        source: Option<&VMGcRef>,
    ) {
        // Increment the ref count of the object being written into the slot.
        if let Some(src) = source {
            self.inc_ref(src);
        }

        // Decrement the ref count of the value being overwritten and, if
        // necessary, deallocate the GC object.
        if let Some(dest) = destination {
            self.dec_ref_and_maybe_dealloc(host_data_table, dest);
        }

        // Do the actual write.
        *destination = source.map(|s| s.unchecked_copy());
    }

    fn expose_gc_ref_to_wasm(&mut self, gc_ref: VMGcRef) {
        // Read the current list head before borrowing through index_mut.
        let next = self.over_approximated_stack_roots
            .as_ref()
            .map(|r| r.unchecked_copy());
        let header = self.index_mut(drc_ref(&gc_ref));
        if header.is_in_over_approximated_stack_roots() {
            return;
        }
        // Push this object onto the head of the over-approximated-stack-roots
        // list using a single index_mut call.
        header.set_in_over_approximated_stack_roots_bit(true);
        header.set_next_over_approximated_stack_root(next);
        self.over_approximated_stack_roots = Some(gc_ref);
    }

    fn alloc_externref(
        &mut self,
        host_data: ExternRefHostDataId,
    ) -> Result<Result<VMExternRef, u64>> {
        let gc_ref =
            match self.alloc_raw(VMGcHeader::externref(), Layout::new::<VMDrcExternRef>())? {
                Err(n) => return Ok(Err(n)),
                Ok(gc_ref) => gc_ref,
            };
        self.index_mut::<VMDrcExternRef>(gc_ref.as_typed_unchecked())
            .host_data = host_data;
        Ok(Ok(gc_ref.into_externref_unchecked()))
    }

    fn externref_host_data(&self, externref: &VMExternRef) -> ExternRefHostDataId {
        let typed_ref = externref_to_drc(externref);
        self.index(typed_ref).host_data
    }

    fn header(&self, gc_ref: &VMGcRef) -> &VMGcHeader {
        let header: &VMGcHeader = self.index(gc_ref.as_typed_unchecked());

        debug_assert!(
            VMGcKind::try_from_u32(header.kind().as_u32()).is_some(),
            "header: invalid VMGcKind {:#010x} at gc_ref {gc_ref:#p}",
            header.kind().as_u32(),
        );

        header
    }

    fn header_mut(&mut self, gc_ref: &VMGcRef) -> &mut VMGcHeader {
        let header: &mut VMGcHeader = self.index_mut(gc_ref.as_typed_unchecked());

        debug_assert!(
            VMGcKind::try_from_u32(header.kind().as_u32()).is_some(),
            "header_mut: invalid VMGcKind {:#010x} at gc_ref {gc_ref:#p}",
            header.kind().as_u32(),
        );

        header
    }

    fn object_size(&self, gc_ref: &VMGcRef) -> usize {
        self.index(drc_ref(gc_ref)).object_size()
    }

    fn alloc_raw(&mut self, header: VMGcHeader, layout: Layout) -> Result<Result<VMGcRef, u64>> {
        debug_assert!(layout.size() >= core::mem::size_of::<VMDrcHeader>());
        debug_assert!(layout.align() >= core::mem::align_of::<VMDrcHeader>());
        debug_assert_eq!(header.reserved_u26(), 0);

        // We must have trace info for every GC type that we allocate in this
        // heap. The only kinds of GC objects we allocate that do not have an
        // associated `VMSharedTypeIndex` are `externref`s, and they don't have
        // any GC edges.
        if let Some(ty) = header.ty() {
            self.ensure_trace_info(ty);
        } else {
            debug_assert_eq!(header.kind(), VMGcKind::ExternRef);
        }

        let object_size = u32::try_from(layout.size()).unwrap();
        let alloc_size = FreeList::aligned_size(object_size);

        // SAFETY: free_list is always Some after heap initialization.
        let gc_ref = match unsafe { self.free_list.as_mut().unwrap_unchecked() }.alloc_fast(alloc_size) {
            None => return Ok(Err(u64::try_from(layout.size()).unwrap())),
            // SAFETY: FreeList uses ALIGN=16, so heap indices have bit 0
            // clear and are always valid non-i31 gc refs.
            Some(index) => VMGcRef::from_raw_non_zero_u32(index),
        };

        // Assert that the newly-allocated memory is still filled with the
        // poison pattern, and hasn't been corrupted since deallocation (or
        // initial heap creation).
        if cfg!(gc_zeal) {
            let start = usize::try_from(gc_ref.as_heap_index().unwrap().get()).unwrap();
            let slice = &self.heap_slice()[start..][..layout.size()];
            gc_assert!(
                slice.iter().all(|&b| b == POISON),
                "newly allocated GC object at index {start} is not fully poisoned; \
                 freed memory was corrupted",
            );
        }

        // Use direct pointer access to skip index_mut bounds checks.
        // SAFETY: gc_ref was just allocated with at least VMDrcHeader size,
        // and is a non-i31 heap ref (FreeList uses ALIGN=16).
        let start = unsafe { gc_ref.as_heap_index().unwrap_unchecked() }.get() as usize;
        // SAFETY: vmmemory is always Some when the heap is attached.
        let vmmemory = unsafe { self.vmmemory.as_ref().unwrap_unchecked() };
        let heap_base = vmmemory.base.as_ptr();
        unsafe {
            *(heap_base.add(start) as *mut VMDrcHeader) = VMDrcHeader {
                header,
                ref_count: 1,
                next_over_approximated_stack_root: None,
                object_size,
            };
        }
        Ok(Ok(gc_ref))
    }

    fn alloc_raw_and_expose(
        &mut self,
        header: VMGcHeader,
        layout: Layout,
    ) -> Result<Result<core::num::NonZeroU32, u64>> {
        debug_assert!(layout.size() >= core::mem::size_of::<VMDrcHeader>());
        debug_assert!(layout.align() >= core::mem::align_of::<VMDrcHeader>());
        debug_assert_eq!(header.reserved_u26(), 0);

        if let Some(ty) = header.ty() {
            self.ensure_trace_info(ty);
        }

        let object_size = layout.size() as u32;
        let alloc_size = FreeList::aligned_size(object_size);

        let gc_ref = match unsafe { self.free_list.as_mut().unwrap_unchecked() }.alloc_fast(alloc_size) {
            None => return Ok(Err(layout.size() as u64)),
            Some(index) => VMGcRef::from_raw_non_zero_u32(index),
        };

        if cfg!(gc_zeal) {
            let start = usize::try_from(gc_ref.as_heap_index().unwrap().get()).unwrap();
            let slice = &self.heap_slice()[start..][..layout.size()];
            gc_assert!(
                slice.iter().all(|&b| b == POISON),
                "newly allocated GC object at index {start} is not fully poisoned; \
                 freed memory was corrupted",
            );
        }

        let raw = gc_ref.as_raw_non_zero_u32();

        // Write header with OASR bit pre-set and next pointer already filled,
        // avoiding the read-modify-write cycle of setting the bit after writing.
        let next = self.over_approximated_stack_roots
            .as_ref()
            .map(|r| r.unchecked_copy());

        // Pre-set the OASR bit on the header before writing to heap.
        // SAFETY: HEADER_IN_OVER_APPROX_LIST_BIT fits in 26 bits and the
        // header's reserved bits are asserted to be 0 above.
        let mut oasr_header = header;
        unsafe {
            oasr_header.unchecked_set_reserved_u26(
                wasmtime_environ::drc::HEADER_IN_OVER_APPROX_LIST_BIT,
            );
        }

        // Use direct pointer access to skip index_mut bounds checks.
        // SAFETY: gc_ref was just allocated with at least VMDrcHeader size,
        // and the heap memory is disjoint from self's struct fields.
        // vmmemory is always Some when the heap is attached (which it must be
        // for allocation to succeed).
        let start = unsafe { gc_ref.as_heap_index().unwrap_unchecked() }.get() as usize;
        let vmmemory = unsafe { self.vmmemory.as_ref().unwrap_unchecked() };
        let heap_base = vmmemory.base.as_ptr();
        let drc_header =
            unsafe { &mut *(heap_base.add(start) as *mut VMDrcHeader) };
        *drc_header = VMDrcHeader {
            header: oasr_header,
            ref_count: 1,
            next_over_approximated_stack_root: next,
            object_size,
        };
        self.over_approximated_stack_roots = Some(gc_ref);

        Ok(Ok(raw))
    }

    fn alloc_uninit_struct_or_exn(
        &mut self,
        ty: VMSharedTypeIndex,
        layout: &GcStructLayout,
    ) -> Result<Result<VMGcRef, u64>> {
        let kind = if layout.is_exception {
            VMGcKind::ExnRef
        } else {
            VMGcKind::StructRef
        };
        let gc_ref =
            match self.alloc_raw(VMGcHeader::from_kind_and_index(kind, ty), layout.layout())? {
                Err(n) => return Ok(Err(n)),
                Ok(gc_ref) => gc_ref,
            };

        Ok(Ok(gc_ref))
    }

    fn dealloc_uninit_struct_or_exn(&mut self, gcref: VMGcRef) {
        self.dealloc(gcref);
    }

    fn alloc_uninit_array(
        &mut self,
        ty: VMSharedTypeIndex,
        length: u32,
        layout: &GcArrayLayout,
    ) -> Result<Result<VMArrayRef, u64>> {
        let gc_ref = match self.alloc_raw(
            VMGcHeader::from_kind_and_index(VMGcKind::ArrayRef, ty),
            layout.layout(length),
        )? {
            Err(n) => return Ok(Err(n)),
            Ok(gc_ref) => gc_ref,
        };

        self.index_mut(gc_ref.as_typed_unchecked::<VMDrcArrayHeader>())
            .length = length;

        Ok(Ok(gc_ref.into_arrayref_unchecked()))
    }

    fn dealloc_uninit_array(&mut self, arrayref: VMArrayRef) {
        self.dealloc(arrayref.into())
    }

    fn array_len(&self, arrayref: &VMArrayRef) -> u32 {
        debug_assert!(arrayref.as_gc_ref().is_typed::<VMDrcArrayHeader>(self));
        self.index::<VMDrcArrayHeader>(arrayref.as_gc_ref().as_typed_unchecked())
            .length
    }

    fn gc<'a>(
        &'a mut self,
        roots: GcRootsIter<'a>,
        host_data_table: &'a mut ExternRefHostDataTable,
    ) -> Box<dyn GarbageCollection<'a> + 'a> {
        assert_eq!(self.no_gc_count, 0, "Cannot GC inside a no-GC scope!");
        Box::new(DrcCollection {
            roots,
            host_data_table,
            heap: self,
            phase: DrcCollectionPhase::Trace,
        })
    }

    unsafe fn vmctx_gc_heap_data(&self) -> NonNull<u8> {
        let ptr: NonNull<Option<VMGcRef>> = NonNull::from(&self.over_approximated_stack_roots);
        ptr.cast()
    }

    fn take_memory(&mut self) -> crate::vm::Memory {
        debug_assert!(self.is_attached());
        self.vmmemory.take();
        self.memory.take().unwrap()
    }

    unsafe fn replace_memory(&mut self, memory: crate::vm::Memory, delta_bytes_grown: u64) {
        debug_assert!(self.memory.is_none());
        debug_assert!(!memory.is_shared_memory());
        self.vmmemory = Some(memory.vmmemory());
        self.memory = Some(memory);

        // Poison the newly-grown region so stale accesses are detectable.
        if cfg!(gc_zeal) {
            let old_cap = self.free_list.as_ref().unwrap().current_capacity();
            let new_bytes = usize::try_from(delta_bytes_grown).unwrap();
            let slice = self.heap_slice_mut();
            if old_cap + new_bytes <= slice.len() {
                slice[old_cap..old_cap + new_bytes].fill(POISON);
            }
        }

        self.free_list
            .as_mut()
            .unwrap()
            .add_capacity(usize::try_from(delta_bytes_grown).unwrap())
    }

    #[inline]
    fn vmmemory(&self) -> VMMemoryDefinition {
        debug_assert!(self.is_attached());
        debug_assert!(!self.memory.as_ref().unwrap().is_shared_memory());
        let vmmemory = self.vmmemory.as_ref().unwrap();
        VMMemoryDefinition {
            base: vmmemory.base,
            current_length: AtomicUsize::new(vmmemory.current_length()),
        }
    }
}

struct DrcCollection<'a> {
    roots: GcRootsIter<'a>,
    host_data_table: &'a mut ExternRefHostDataTable,
    heap: &'a mut DrcHeap,
    phase: DrcCollectionPhase,
}

enum DrcCollectionPhase {
    Trace,
    Sweep,
    Done,
}

impl<'a> GarbageCollection<'a> for DrcCollection<'a> {
    fn collect_increment(&mut self) -> GcProgress {
        match self.phase {
            DrcCollectionPhase::Trace => {
                self.heap.assert_over_approximated_stack_roots_integrity();
                self.heap.assert_free_blocks_are_poisoned();

                self.heap.trace(&mut self.roots);

                self.heap.assert_over_approximated_stack_roots_integrity();
                self.heap.assert_free_blocks_are_poisoned();

                self.phase = DrcCollectionPhase::Sweep;
                GcProgress::Continue
            }
            DrcCollectionPhase::Sweep => {
                self.heap.assert_over_approximated_stack_roots_integrity();
                self.heap.assert_free_blocks_are_poisoned();

                self.heap.sweep(self.host_data_table);

                self.heap.assert_over_approximated_stack_roots_integrity();
                self.heap.assert_free_blocks_are_poisoned();

                self.phase = DrcCollectionPhase::Done;
                GcProgress::Complete
            }
            DrcCollectionPhase::Done => GcProgress::Complete,
        }
    }
}

#[derive(Debug, Default)]
struct DebugOnly<T> {
    inner: T,
}

impl<T> Deref for DebugOnly<T> {
    type Target = T;

    fn deref(&self) -> &T {
        if cfg!(debug_assertions) {
            &self.inner
        } else {
            panic!(
                "only deref `DebugOnly` when `cfg(debug_assertions)` or \
                 inside a `debug_assert!(..)`"
            )
        }
    }
}

impl<T> DerefMut for DebugOnly<T> {
    fn deref_mut(&mut self) -> &mut T {
        if cfg!(debug_assertions) {
            &mut self.inner
        } else {
            panic!(
                "only deref `DebugOnly` when `cfg(debug_assertions)` or \
                 inside a `debug_assert!(..)`"
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmtime_environ::HostPtr;

    #[test]
    fn vm_drc_header_size_align() {
        assert_eq!(
            (wasmtime_environ::drc::HEADER_SIZE as usize),
            core::mem::size_of::<VMDrcHeader>()
        );
        assert_eq!(
            (wasmtime_environ::drc::HEADER_ALIGN as usize),
            core::mem::align_of::<VMDrcHeader>()
        );
    }

    #[test]
    fn vm_drc_array_header_length_offset() {
        assert_eq!(
            wasmtime_environ::drc::ARRAY_LENGTH_OFFSET,
            u32::try_from(core::mem::offset_of!(VMDrcArrayHeader, length)).unwrap(),
        );
    }

    #[test]
    fn ref_count_is_at_correct_offset() {
        let extern_data = VMDrcHeader {
            header: VMGcHeader::externref(),
            ref_count: 0,
            next_over_approximated_stack_root: None,
            object_size: 0,
        };

        let extern_data_ptr = &extern_data as *const _;
        let ref_count_ptr = &extern_data.ref_count as *const _;

        let actual_offset = (ref_count_ptr as usize) - (extern_data_ptr as usize);

        let offsets = wasmtime_environ::VMOffsets::from(wasmtime_environ::VMOffsetsFields {
            ptr: HostPtr,
            num_imported_functions: 0,
            num_imported_tables: 0,
            num_imported_memories: 0,
            num_imported_globals: 0,
            num_imported_tags: 0,
            num_defined_tables: 0,
            num_defined_memories: 0,
            num_owned_memories: 0,
            num_defined_globals: 0,
            num_defined_tags: 0,
            num_escaped_funcs: 0,
        });

        assert_eq!(
            offsets.vm_drc_header_ref_count(),
            u32::try_from(actual_offset).unwrap(),
        );
    }
}
