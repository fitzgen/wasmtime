//! Shared translation of the component model's `{enter,exit}-sync-call`
//! intrinsics, and of the `resource.transfer-borrow-to-owner` that fused
//! adapters make between them.
//!
//! Two different places wrap a synchronous call into another component
//! instance with these intrinsics: fused adapters, whose calls are translated
//! in `func_environ.rs`, and the `resource.drop` trampoline's call to a
//! resource destructor, translated in `compiler/component.rs`.
//!
//! Rather than call out to the host to do the task bookkeeping that entering a
//! sync call entails, both push a `VMDeferredThread` into their own stack frame
//! and publish it as the store's current thread. The host then only promotes it
//! into a real `GuestTask` if it ever actually needs one; see
//! `StoreOpaque::force_current_thread` and
//! `StoreOpaque::force_deferred_current_thread`.
//!
//! The two callers differ in where the arguments recorded by `enter` come from
//! and in how they branch between `exit`'s fast and slow paths. The
//! `resource.drop` trampoline uses [`exit`], which builds its own diamond. A
//! fused adapter instead does its own branching in Wasm, calling the guard
//! [`deferred_thread_is_current`], the fast path [`exit_fast`], and the out-of-line
//! `exit-sync-call` libcall as separate intrinsics. Everything else lives here:
//! the `VMDeferredThread` layout and, in particular, the `context.{get,set}`
//! slot save/zero/restore, which has to match what the host does for a
//! non-deferred thread in `StoreOpaque::set_thread`.
//!
//! Between `enter` and `exit`, a fused adapter lending a `borrow` to the
//! resource's owning instance uses the fast path
//! [`transfer_borrow_to_owner_fast`] when [`deferred_thread_is_current`] and each
//! `transfer_borrow_to_owner_*` guard hold. Instead of creating a real lend, it
//! records the handle in the store's lazy lenders, which `exit` discards
//! wholesale and the host promotes into real lends only if it forces the
//! deferred thread. Anything else, such as a promoted thread, a full lazy
//! lenders array, or an invalid handle, uses the libcall instead.

use crate::alias_region::AliasRegions;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use wasmtime_environ::{
    GetPtrSize, LAZY_LENDERS_CAPACITY, NUM_COMPONENT_CONTEXT_SLOTS, PtrSize,
    component::{TypeResourceTableIndex, VMComponentOffsets},
};

/// The `enter_sync_call` arguments recorded into the pushed
/// `VMDeferredThread`, to be replayed by the host if it ever has to promote the
/// deferred thread into a real one.
pub struct EnterArgs {
    /// Whether the callee is async-lifted, as an `i32` boolean.
    pub callee_async: ir::Value,
    /// The component instance being called into.
    pub callee_instance: ir::Value,
}

/// Translates `enter-sync-call`: allocates a `VMDeferredThread` in the current
/// function's frame, records `args` into it along with the caller's context
/// slots, zeroes the live context slots for the callee, and publishes the frame
/// as the store's current thread.
///
/// `vmctx` must be a core wasm `*mut VMContext` for this store. The returned
/// stack slot must be handed to [`exit`] once the callee returns.
pub fn enter<O>(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<O>,
    vmctx: ir::Value,
    args: EnterArgs,
) -> ir::StackSlot
where
    O: GetPtrSize,
{
    let pointer_type = alias_regions.pointer_type();
    let ptr = alias_regions.ptr_size();

    // Allocate the on-stack `VMDeferredThread`.
    let slot = builder.func.create_sized_stack_slot(ir::StackSlotData::new(
        ir::StackSlotKind::ExplicitSlot,
        u32::from(ptr.vm_deferred_thread().size()),
        u8::try_from(ptr.size().trailing_zeros()).unwrap(),
    ));
    let slot_addr = builder.ins().stack_addr(pointer_type, slot, 0);
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);

    // Link the previous current thread in as this frame's parent.
    let parent = alias_regions
        .vm_store_context()
        .current_thread()
        .load(&mut builder.cursor(), vmstore);
    alias_regions
        .vm_deferred_thread()
        .parent()
        .store(&mut builder.cursor(), slot_addr, parent);

    // Record the deferred `enter_sync_call` arguments.
    alias_regions.vm_deferred_thread().callee_async().store(
        &mut builder.cursor(),
        slot_addr,
        args.callee_async,
    );
    alias_regions.vm_deferred_thread().callee_instance().store(
        &mut builder.cursor(),
        slot_addr,
        args.callee_instance,
    );

    // Save the caller's context slots into the frame and reset the live values
    // to 0 for the freshly-entered (deferred) thread.
    for i in 0..u8::try_from(NUM_COMPONENT_CONTEXT_SLOTS).unwrap() {
        let saved = alias_regions
            .vm_store_context()
            .component_context(i)
            .load(&mut builder.cursor(), vmstore);
        alias_regions.vm_deferred_thread().saved_context(i).store(
            &mut builder.cursor(),
            slot_addr,
            saved,
        );
        let zero = builder.ins().iconst(ir::types::I32, 0);
        alias_regions.vm_store_context().component_context(i).store(
            &mut builder.cursor(),
            vmstore,
            zero,
        );
    }

    // Record where this frame's lazy lends will start, so that `exit` can
    // discard them and the host can find exactly this frame's entries if it
    // has to promote them into real lends.
    let lazy_lenders_len = alias_regions
        .vm_store_context()
        .lazy_lenders_len()
        .load(&mut builder.cursor(), vmstore);
    alias_regions
        .vm_deferred_thread()
        .lazy_lenders_start()
        .store(&mut builder.cursor(), slot_addr, lazy_lenders_len);

    // Publish the deferred thread as the store's current thread.
    alias_regions.vm_store_context().current_thread().store(
        &mut builder.cursor(),
        vmstore,
        slot_addr,
    );

    slot
}

/// The unfinished slow path of an [`exit`], which the caller must fill in with
/// an out-of-line call to the `exit_sync_call` intrinsic before finishing it
/// with [`SlowExit::finish`].
#[must_use = "the slow path of an exit-sync-call must be emitted and finished"]
pub struct SlowExit {
    cont_block: ir::Block,
}

impl SlowExit {
    pub fn finish(self, builder: &mut FunctionBuilder<'_>) {
        builder.ins().jump(self.cont_block, &[]);
        builder.seal_block(self.cont_block);
        builder.switch_to_block(self.cont_block);
    }
}

/// Translates `exit-sync-call`, the counterpart to [`enter`], where `slot` is
/// the stack slot that `enter` returned.
///
/// If the deferred thread pushed by `enter` is still the store's current
/// thread, meaning nothing ever forced it into a real one, then it is popped,
/// its lazy lends discarded, and the caller's context slots restored inline.
/// Otherwise the host has state to tear down, which only it can do, so the
/// caller must emit an out-of-line call to the `exit_sync_call` intrinsic: on
/// return the builder is positioned in that slow path, which is finished with
/// [`SlowExit::finish`].
pub fn exit<O>(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<O>,
    vmctx: ir::Value,
    slot: ir::StackSlot,
) -> SlowExit
where
    O: GetPtrSize,
{
    let is_current = deferred_thread_is_current(builder, alias_regions, vmctx, slot);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();
    builder
        .ins()
        .brif(is_current, fast_block, &[], slow_block, &[]);
    builder.seal_block(fast_block);
    builder.seal_block(slow_block);

    builder.switch_to_block(fast_block);
    exit_fast(builder, alias_regions, vmctx, slot);
    builder.ins().jump(cont_block, &[]);

    // Slow path: the thread was promoted to a real one, so the caller does the
    // equivalent teardown out-of-line.
    builder.switch_to_block(slow_block);
    builder.set_cold_block(slow_block);
    SlowExit { cont_block }
}

/// Whether the `VMDeferredThread` in `slot`, which [`enter`] pushed, is still
/// the store's current thread, meaning that nothing has forced it into a real
/// thread.
///
/// This is the guard of both [`exit_fast`] and [`transfer_borrow_to_owner_fast`].
pub fn deferred_thread_is_current<O>(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<O>,
    vmctx: ir::Value,
    slot: ir::StackSlot,
) -> ir::Value
where
    O: GetPtrSize,
{
    let pointer_type = alias_regions.pointer_type();
    let slot_addr = builder.ins().stack_addr(pointer_type, slot, 0);
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);
    let cur = alias_regions
        .vm_store_context()
        .current_thread()
        .load(&mut builder.cursor(), vmstore);
    builder.ins().icmp(IntCC::Equal, cur, slot_addr)
}

/// The fast path of [`exit`]: pops the deferred thread in `slot`, discards its
/// lazy lends, and restores the caller's context slots.
///
/// Only valid when [`deferred_thread_is_current`] holds.
pub fn exit_fast<O>(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<O>,
    vmctx: ir::Value,
    slot: ir::StackSlot,
) where
    O: GetPtrSize,
{
    let pointer_type = alias_regions.pointer_type();
    let slot_addr = builder.ins().stack_addr(pointer_type, slot, 0);
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);

    let parent = alias_regions
        .vm_deferred_thread()
        .parent()
        .load(&mut builder.cursor(), slot_addr);
    alias_regions
        .vm_store_context()
        .current_thread()
        .store(&mut builder.cursor(), vmstore, parent);

    // Discard the lazy lends this frame recorded. We can do this by simply
    // resetting the `lazy_lenders_len`, and without any per-lend work, since
    // the deferred thread and its lazy lends were never materialized.
    let start = alias_regions
        .vm_deferred_thread()
        .lazy_lenders_start()
        .load(&mut builder.cursor(), slot_addr);
    alias_regions.vm_store_context().lazy_lenders_len().store(
        &mut builder.cursor(),
        vmstore,
        start,
    );

    for i in 0..u8::try_from(NUM_COMPONENT_CONTEXT_SLOTS).unwrap() {
        let saved = alias_regions
            .vm_deferred_thread()
            .saved_context(i)
            .load(&mut builder.cursor(), slot_addr);
        alias_regions.vm_store_context().component_context(i).store(
            &mut builder.cursor(),
            vmstore,
            saved,
        );
    }
}

/// The handle table and handle that a FACT adapter's versioned
/// `resource.transfer-borrow-to-owner` pieces operate on.
pub struct TransferBorrow {
    /// The `VMComponentContext` that owns the handle table.
    pub vmcomponent: ir::Value,
    /// The offset of the handle table within `vmcomponent`.
    pub table_offset: u32,
    /// The 1-based handle being lent.
    pub index: ir::Value,
}

/// A guard of `resource.transfer-borrow-to-owner`: whether the store's lazy
/// lenders have room for another lazy lend.
pub fn transfer_borrow_to_owner_has_room(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    vmctx: ir::Value,
) -> ir::Value {
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);
    let (_len, has_room) = lazy_lenders_has_room(builder, alias_regions, vmstore);
    has_room
}

/// A guard of `resource.transfer-borrow-to-owner`: whether the handle is in
/// bounds of its handle table.
pub fn transfer_borrow_to_owner_in_bounds(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    transfer: &TransferBorrow,
) -> ir::Value {
    let pointer_type = alias_regions.pointer_type();
    // Handle 0 wraps to `u32::MAX` and fails the bounds check.
    let slot_index = builder.ins().iadd_imm_s(transfer.index, -1);
    let slot_index = crate::cast_to_pointer(builder, pointer_type, slot_index, false);
    let slots_len = alias_regions
        .vm_handle_table()
        .current_slots()
        .relative_to(transfer.table_offset)
        .load(&mut builder.cursor(), transfer.vmcomponent);
    builder
        .ins()
        .icmp(IntCC::UnsignedLessThan, slot_index, slots_len)
}

/// A guard of `resource.transfer-borrow-to-owner`: whether the handle's slot
/// holds an owned or borrowed resource.
///
/// Only valid when [`transfer_borrow_to_owner_in_bounds`] holds.
pub fn transfer_borrow_to_owner_is_resource(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    transfer: &TransferBorrow,
) -> ir::Value {
    let layout = HandleTableSlotLayout::new(alias_regions);
    let slot_ptr = transfer_slot_ptr(builder, alias_regions, &layout, transfer);
    // The own and borrow tags are adjacent, so one unsigned comparison checks
    // for either.
    let tag = alias_regions
        .vm_handle_table_slot()
        .tag()
        .load(&mut builder.cursor(), slot_ptr);
    let tag = builder.ins().iadd_imm_s(tag, -i64::from(layout.own_tag));
    builder.ins().icmp_imm_u(
        IntCC::UnsignedLessThanOrEqual,
        tag,
        i64::from(layout.borrow_tag - layout.own_tag),
    )
}

/// A guard of `resource.transfer-borrow-to-owner`: whether the handle's
/// resource has type `ty`.
///
/// Only valid when [`transfer_borrow_to_owner_is_resource`] holds.
pub fn transfer_borrow_to_owner_has_type(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    transfer: &TransferBorrow,
    ty: TypeResourceTableIndex,
) -> ir::Value {
    let layout = HandleTableSlotLayout::new(alias_regions);
    let slot_ptr = transfer_slot_ptr(builder, alias_regions, &layout, transfer);
    let slot_ty = alias_regions
        .vm_typed_resource()
        .ty()
        .relative_to(layout.resource_offset)
        .load(&mut builder.cursor(), slot_ptr);
    builder
        .ins()
        .icmp_imm_u(IntCC::Equal, slot_ty, i64::from(ty.as_u32()))
}

/// The fast path of `resource.transfer-borrow-to-owner`: records a lazy lend
/// of the handle, of type `ty`, and returns its resource's `rep`.
///
/// This is the inline equivalent of `ResourceTables::resource_lift_borrow_lazy`
/// and is only valid when [`deferred_thread_is_current`] and all of the
/// `transfer_borrow_to_owner_*` guards hold.
pub fn transfer_borrow_to_owner_fast(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    vmctx: ir::Value,
    transfer: &TransferBorrow,
    ty: TypeResourceTableIndex,
) -> ir::Value {
    let layout = HandleTableSlotLayout::new(alias_regions);
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);
    let slot_ptr = transfer_slot_ptr(builder, alias_regions, &layout, transfer);
    let rep = alias_regions
        .vm_typed_resource()
        .rep()
        .relative_to(layout.resource_offset)
        .load(&mut builder.cursor(), slot_ptr);

    // Record the lend at the end of the store's lazy lenders, for the host to
    // promote if it forces the current thread. The `has_room` guard already
    // checked that there is room.
    let len = alias_regions
        .vm_store_context()
        .lazy_lenders_len()
        .load(&mut builder.cursor(), vmstore);
    let entry_size = i64::from(alias_regions.ptr_size().vm_resource_index().size());
    let lazy_lenders = alias_regions
        .vm_store_context()
        .lazy_lenders()
        .load(&mut builder.cursor(), vmstore);
    let entry_offset = builder.ins().imul_imm_u(len, entry_size);
    let entry_addr = builder.ins().iadd(lazy_lenders, entry_offset);
    let entry_ty = builder.ins().iconst(ir::types::I32, i64::from(ty.as_u32()));
    alias_regions
        .vm_resource_index()
        .ty()
        .store(&mut builder.cursor(), entry_addr, entry_ty);
    alias_regions.vm_resource_index().index().store(
        &mut builder.cursor(),
        entry_addr,
        transfer.index,
    );
    let next_len = builder.ins().iadd_imm_s(len, 1);
    alias_regions.vm_store_context().lazy_lenders_len().store(
        &mut builder.cursor(),
        vmstore,
        next_len,
    );

    rep
}

/// The address of the handle's slot, which must already be bounds checked.
fn transfer_slot_ptr(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    layout: &HandleTableSlotLayout,
    transfer: &TransferBorrow,
) -> ir::Value {
    let pointer_type = alias_regions.pointer_type();
    let slot_index = builder.ins().iadd_imm_s(transfer.index, -1);
    let slot_index = crate::cast_to_pointer(builder, pointer_type, slot_index, false);
    handle_table_slot_addr(
        builder,
        alias_regions,
        transfer.vmcomponent,
        transfer.table_offset,
        slot_index,
        layout.size,
    )
}

/// The layout of a `VMHandleTableSlot` that the inline resource fast paths
/// rely on.
struct HandleTableSlotLayout {
    /// The size of a slot, in bytes.
    size: i64,
    /// The offset of the `VMTypedResource` within either resource variant.
    resource_offset: u32,
    /// The tag of the `resource_own` variant.
    own_tag: u32,
    /// The tag of the `resource_borrow` variant.
    borrow_tag: u32,
}

impl HandleTableSlotLayout {
    fn new(alias_regions: &AliasRegions<VMComponentOffsets<u8>>) -> Self {
        let slot = alias_regions.ptr_size().vm_handle_table_slot();
        debug_assert_eq!(
            slot.resource_own().resource(),
            slot.resource_borrow().resource(),
            "both resource variants must put `resource` at the same offset, \
             so that the fast path need not branch on the tag",
        );
        let own_tag = slot.resource_own().tag_value();
        let borrow_tag = slot.resource_borrow().tag_value();
        debug_assert_eq!(borrow_tag, own_tag + 1);
        Self {
            size: i64::from(slot.size()),
            resource_offset: u32::from(slot.resource_own().resource()),
            own_tag,
            borrow_tag,
        }
    }
}

/// Returns the store's current `lazy_lenders_len` and whether there is room to
/// push another lazy lend.
fn lazy_lenders_has_room(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    vmstore: ir::Value,
) -> (ir::Value, ir::Value) {
    let pointer_type = alias_regions.pointer_type();
    let len = alias_regions
        .vm_store_context()
        .lazy_lenders_len()
        .load(&mut builder.cursor(), vmstore);
    let capacity = i64::try_from(LAZY_LENDERS_CAPACITY).unwrap();
    let cap = builder.ins().iconst(pointer_type, capacity);
    let has_room = builder.ins().icmp(IntCC::UnsignedLessThan, len, cap);
    (len, has_room)
}

/// The address of slot `slot_index` in the handle table at `table_offset`,
/// which must already be bounds checked.
fn handle_table_slot_addr(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    vmcomponent: ir::Value,
    table_offset: u32,
    slot_index: ir::Value,
    slot_size: i64,
) -> ir::Value {
    let slots = alias_regions
        .vm_handle_table()
        .base()
        .relative_to(table_offset)
        .load(&mut builder.cursor(), vmcomponent);
    let slot_offset = builder.ins().imul_imm_u(slot_index, slot_size);
    builder.ins().iadd(slots, slot_offset)
}
