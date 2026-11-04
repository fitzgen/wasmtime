//! Shared translation of the component model's `{enter,exit}-sync-call`
//! intrinsics.
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
//! The two callers differ only in where the arguments recorded by `enter` come
//! from and in how `exit`'s out-of-line slow path is called. Everything else
//! lives here: the `VMDeferredThread` layout and, in particular, the
//! `context.{get,set}` slot save/zero/restore, which has to match what the host
//! does for a non-deferred thread in `StoreOpaque::set_thread`.

use crate::alias_region::AliasRegions;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_frontend::FunctionBuilder;
use wasmtime_environ::{
    GetPtrSize, LAZY_LENDERS_CAPACITY, NUM_COMPONENT_CONTEXT_SLOTS, PtrSize,
    component::{RuntimeComponentInstanceIndex, TypeResourceTableIndex, VMComponentOffsets},
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
    let is_current = builder.ins().icmp(IntCC::Equal, cur, slot_addr);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let cont_block = builder.create_block();
    builder
        .ins()
        .brif(is_current, fast_block, &[], slow_block, &[]);
    builder.seal_block(fast_block);
    builder.seal_block(slow_block);

    // Fast path: pop the deferred thread and restore the caller's context.
    builder.switch_to_block(fast_block);
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
    builder.ins().jump(cont_block, &[]);

    // Slow path: the thread was promoted to a real one, so the caller does the
    // equivalent teardown out-of-line.
    builder.switch_to_block(slow_block);
    SlowExit { cont_block }
}

/// Translates `resource.transfer-borrow-to-owner`: validates the handle,
/// records it in `lazy_lenders`, and yields the resource's "rep", without
/// calling into the host.
///
/// This is the inline counterpart of `ResourceTables::resource_lift_borrow_lazy`
/// and, like it, leaves the handle's `lend_count` alone: forcing the thread
/// promotes the recorded entry into a real lend, and `exit`'s fast path
/// discards it by resetting `lazy_lenders_len`.
///
/// `vmcomponent` is the `VMComponentContext` the intrinsic call receives as
/// its callee vmctx, `slot` the enclosing frame's `VMDeferredThread`, `ty` the
/// handle's resource type, and `instance` the component instance owning that
/// type's handle table.
///
/// On return the builder sits in the slow path, which the caller fills in with
/// the libcall before jumping to the returned join block, whose parameter is
/// the "rep".
///
/// # The guard
///
/// The inline path runs only while this frame's deferred thread is still
/// current, `lazy_lenders` has room, the handle is in bounds, and its slot
/// holds a resource of type `ty`.
///
/// The first condition is load-bearing rather than an optimization: once
/// something has forced the thread partway through parameter translation, an
/// entry recorded afterwards would never be promoted into the real
/// `CallContext`, so the lend would go untracked. An `own` transfer earlier in
/// the same signature forces it.
///
/// Every failure routes to the libcall rather than trapping inline, so the
/// host's specific diagnostics survive and the inline path has no trap edges.
pub fn transfer_borrow_to_owner(
    builder: &mut FunctionBuilder<'_>,
    alias_regions: &mut AliasRegions<VMComponentOffsets<u8>>,
    vmctx: ir::Value,
    vmcomponent: ir::Value,
    slot: ir::StackSlot,
    ty: TypeResourceTableIndex,
    instance: RuntimeComponentInstanceIndex,
    index: ir::Value,
) -> ir::Block {
    let pointer_type = alias_regions.pointer_type();
    // Copied out up front: the accessors below borrow `alias_regions` mutably.
    let (slot_size, entry_size, resource_offset, own_tag, borrow_tag) = {
        let ptr = alias_regions.ptr_size();
        let slot = ptr.vm_handle_table_slot();
        debug_assert_eq!(
            slot.resource_own().resource(),
            slot.resource_borrow().resource(),
            "both resource variants must put `resource` at the same offset, \
             so that the fast path need not branch on the tag",
        );
        let own_tag = slot.resource_own().tag_value();
        let borrow_tag = slot.resource_borrow().tag_value();
        debug_assert_eq!(borrow_tag, own_tag + 1);
        (
            i64::from(slot.size()),
            i64::from(ptr.vm_resource_index().size()),
            u32::from(slot.resource_own().resource()),
            own_tag,
            borrow_tag,
        )
    };
    let slot_addr = builder.ins().stack_addr(pointer_type, slot, 0);
    let vmstore = alias_regions
        .vmctx()
        .store_context()
        .load(&mut builder.cursor(), vmctx);

    // A constant offset: the trampoline bakes in the source type, and so the
    // instance.
    let table_offset = alias_regions.offsets().handle_tables().at(instance);

    let cur = alias_regions
        .vm_store_context()
        .current_thread()
        .load(&mut builder.cursor(), vmstore);
    let is_current = builder.ins().icmp(IntCC::Equal, cur, slot_addr);

    let len = alias_regions
        .vm_store_context()
        .lazy_lenders_len()
        .load(&mut builder.cursor(), vmstore);
    let capacity = i64::try_from(LAZY_LENDERS_CAPACITY).unwrap();
    let cap = builder.ins().iconst(pointer_type, capacity);
    let has_room = builder.ins().icmp(IntCC::UnsignedLessThan, len, cap);
    let ok = builder.ins().band(is_current, has_room);

    // A handle is one more than its slot's position; index 0 underflows to
    // `u32::MAX`, which the bounds check below rejects.
    let slot_index = builder.ins().iadd_imm_s(index, -1);
    let slot_index = builder.ins().uextend(pointer_type, slot_index);
    let slots_len = alias_regions
        .vm_handle_table()
        .current_slots()
        .relative_to(table_offset)
        .load(&mut builder.cursor(), vmcomponent);
    let in_bounds = builder
        .ins()
        .icmp(IntCC::UnsignedLessThan, slot_index, slots_len);
    let ok = builder.ins().band(ok, in_bounds);

    let check_block = builder.create_block();
    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let join_block = builder.create_block();
    builder.append_block_param(join_block, ir::types::I32);
    builder.ins().brif(ok, check_block, &[], slow_block, &[]);
    builder.seal_block(check_block);

    // The slot is in bounds, so it is safe to read; check that it holds a
    // resource of the expected type, as `HandleTable::resource_lend_deferred`
    // does.
    builder.switch_to_block(check_block);
    let slots = alias_regions
        .vm_handle_table()
        .base()
        .relative_to(table_offset)
        .load(&mut builder.cursor(), vmcomponent);
    let slot_offset = builder.ins().imul_imm_u(slot_index, slot_size);
    let slot_ptr = builder.ins().iadd(slots, slot_offset);

    // Either resource variant will do, and their tags are adjacent.
    let tag = alias_regions
        .vm_handle_table_slot()
        .tag()
        .load(&mut builder.cursor(), slot_ptr);
    let tag = builder.ins().iadd_imm_s(tag, -i64::from(own_tag));
    let is_resource = builder.ins().icmp_imm_u(
        IntCC::UnsignedLessThanOrEqual,
        tag,
        i64::from(borrow_tag - own_tag),
    );

    let slot_ty = alias_regions
        .vm_typed_resource()
        .ty()
        .relative_to(resource_offset)
        .load(&mut builder.cursor(), slot_ptr);
    let is_ty = builder
        .ins()
        .icmp_imm_u(IntCC::Equal, slot_ty, i64::from(ty.as_u32()));
    let ok = builder.ins().band(is_resource, is_ty);
    builder.ins().brif(ok, fast_block, &[], slow_block, &[]);
    builder.seal_block(fast_block);

    builder.switch_to_block(fast_block);
    let rep = alias_regions
        .vm_typed_resource()
        .rep()
        .relative_to(resource_offset)
        .load(&mut builder.cursor(), slot_ptr);

    // Record the lend for the host to promote if it forces this thread.
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
    alias_regions
        .vm_resource_index()
        .index()
        .store(&mut builder.cursor(), entry_addr, index);
    let next_len = builder.ins().iadd_imm_s(len, 1);
    alias_regions.vm_store_context().lazy_lenders_len().store(
        &mut builder.cursor(),
        vmstore,
        next_len,
    );
    builder.ins().jump(join_block, &[rep.into()]);

    builder.switch_to_block(slow_block);
    builder.seal_block(slow_block);
    join_block
}
