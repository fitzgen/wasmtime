//! Working with GC `struct` objects.

use crate::runtime::vm::VMGcRef;
use crate::store::StoreId;
use crate::vm::GcStructLayout;
use crate::{
    store::{AutoAssertNoGc, StoreOpaque},
    AsContextMut, GcRefImpl, GcRootIndex, HeapType, ManuallyRooted, RefType, Result, RootSet,
    Rooted, ValRaw, ValType, WasmTy,
};
use crate::{AsContext, RootedGcRefImpl, StorageType, StructType, Val};
use anyhow::{ensure, Context};
use core::mem::{self, MaybeUninit};
use wasmtime_environ::{VMGcKind, VMSharedTypeIndex};

/// TODO FITZGEN
pub struct StructAllocator {
    store_id: StoreId,
    ty: StructType,
    type_index: VMSharedTypeIndex,
    layout: GcStructLayout,
}

impl StructAllocator {
    /// TODO FITZGEN
    pub fn new(mut store: impl AsContextMut, ty: StructType) -> Self {
        Self::_new(store.as_context_mut().0, ty)
    }

    pub(crate) fn _new(store: &mut StoreOpaque, ty: StructType) -> Self {
        store.insert_gc_host_alloc_type(ty.registered_type().clone());
        let store_id = store.id();
        let type_index = ty.type_index();

        // TODO FITZGEN: just store this in the registered type?
        let layout = store
            .engine()
            .gc_runtime()
            .struct_layout(ty.as_wasm_struct_type());

        StructAllocator {
            store_id,
            ty,
            type_index,
            layout,
        }
    }
}

/// A reference to a Wasm `struct` instance.
///
/// TODO FITZGEN
///
/// # Example
///
/// TODO FITZGEN
#[derive(Debug)]
#[repr(transparent)]
pub struct StructRef {
    inner: GcRootIndex,
}

unsafe impl GcRefImpl for StructRef {
    #[allow(private_interfaces)]
    fn transmute_ref(index: &GcRootIndex) -> &Self {
        // Safety: `StructRef` is a newtype of a `GcRootIndex`.
        let me: &Self = unsafe { mem::transmute(index) };

        // Assert we really are just a newtype of a `GcRootIndex`.
        assert!(matches!(
            me,
            Self {
                inner: GcRootIndex { .. },
            }
        ));

        me
    }
}

impl StructRef {
    /// TODO FITZGEN
    ///
    /// # Panics
    ///
    /// Panics if the allocator or any of the field values is not associated
    /// with the given store.
    ///
    /// # Example
    ///
    /// TODO FITZGEN
    pub fn new(
        mut store: impl AsContextMut,
        allocator: &StructAllocator,
        fields: &[Val],
    ) -> Result<Rooted<StructRef>> {
        Self::_new(store.as_context_mut().0, allocator, fields)
    }

    pub(crate) fn _new(
        store: &mut StoreOpaque,
        allocator: &StructAllocator,
        fields: &[Val],
    ) -> Result<Rooted<StructRef>> {
        assert_eq!(
            store.id(),
            allocator.store_id,
            "attempted to use an allocator with the wrong store"
        );

        // Type check the given values against the field types.
        let expected_len = allocator.ty.fields().len();
        let actual_len = fields.len();
        ensure!(
            actual_len == expected_len,
            "expected {expected_len} fields, got {actual_len}"
        );
        for (ty, val) in allocator.ty.fields().zip(fields) {
            assert!(val.comes_from_same_store(store));
            let ty = match ty.element_type() {
                StorageType::I8 | StorageType::I16 => &ValType::I32,
                StorageType::ValType(ty) => ty,
            };
            val.ensure_matches_ty(store, &ty)
                .context("field type mismatch")?;
        }

        // Allocate the struct and write each field value into the appropriate
        // offset.
        let structref = store
            .gc_store_mut()?
            .alloc_uninit_struct(allocator.type_index, &allocator.layout)?;
        let mut store = AutoAssertNoGc::new(store);
        for ((offset, ty), val) in allocator
            .layout
            .fields
            .iter()
            .zip(allocator.ty.fields())
            .zip(fields)
        {
            match (ty.element_type(), val) {
                (StorageType::I8, Val::I32(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x as u8);
                }
                (StorageType::I16, Val::I32(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x as u16);
                }
                (StorageType::ValType(_), Val::I32(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x);
                }
                (StorageType::ValType(_), Val::I64(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x);
                }
                (StorageType::ValType(_), Val::F32(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x);
                }
                (StorageType::ValType(_), Val::F64(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x);
                }
                (StorageType::ValType(_), Val::V128(x)) => {
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, *x);
                }
                (StorageType::ValType(_), Val::FuncRef(_)) => {
                    // TODO: we can't trust the GC heap, which means we can't
                    // read native pointers out of it and trust them. That means
                    // we need to do the same side table kind of thing we do
                    // with `externref` host data here. This isn't implemented
                    // yet.
                    unimplemented!("storing funcrefs in the GC heap")
                }
                (StorageType::ValType(_), Val::ExternRef(x)) => {
                    // NB: We don't need to do a write barrier when initializing
                    // a field, just the clone barrier.
                    let x = match x {
                        None => 0,
                        Some(x) => x.try_clone_gc_ref(&mut store)?.as_raw_u32(),
                    };
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, x);
                }
                (StorageType::ValType(_), Val::AnyRef(x)) => {
                    // NB: We don't need to do a write barrier when initializing
                    // a field, just the clone barrier.
                    let x = match x {
                        None => 0,
                        Some(x) => x.try_clone_gc_ref(&mut store)?.as_raw_u32(),
                    };
                    store
                        .gc_store_mut()?
                        .struct_data(&structref, allocator.layout.size)
                        .write_pod(*offset, x);
                }
                _ => unreachable!("did type check above"),
            }
        }

        Ok(Rooted::new(&mut store, structref.into()))
    }

    /// Get this `structref`'s type.
    ///
    /// Returns an `Err(_)` if this reference has been unrooted.
    pub fn ty(&self, store: impl AsContext) -> Result<StructType> {
        self._ty(store.as_context().0)
    }

    pub(crate) fn _ty(&self, store: &StoreOpaque) -> Result<StructType> {
        let gc_ref = self.inner.unchecked_try_gc_ref(store)?;
        let header = store.gc_store()?.header(gc_ref);
        debug_assert!(header.kind().matches(VMGcKind::StructRef));
        Ok(StructType::from_shared_type_index(
            store.engine(),
            header.ty().expect("structrefs should have concrete types"),
        ))
    }

    /// TODO FITZGEN
    ///
    /// Returns an `Err(_)` if this reference has been unrooted.
    pub fn fields(&self, store: impl AsContext) -> Result<impl ExactSizeIterator<Item = Val>> {
        self._fields(store.as_context().0)
    }

    pub(crate) fn _fields(
        &self,
        store: &StoreOpaque,
    ) -> Result<impl ExactSizeIterator<Item = Val>> {
        todo!("FITZGEN");
        #[allow(unreachable_code)]
        {
            Ok(vec![].into_iter())
        }
    }

    /// TODO FITZGEN
    ///
    /// Returns `None` if the index is out of bounds.
    ///
    /// Returns an `Err(_)` if this reference has been unrooted.
    pub fn field(&self, store: impl AsContext, index: usize) -> Result<Option<Val>> {
        self._field(store.as_context().0, index)
    }

    pub(crate) fn _field(&self, store: &StoreOpaque, index: usize) -> Result<Option<Val>> {
        todo!("FITZGEN")
    }

    /// Create a new `Rooted<StructRef>` from the given GC reference.
    ///
    /// `gc_ref` should point to a valid `structref` and should belong to the
    /// store's GC heap. Failure to uphold these invariants is memory safe but
    /// will lead to general incorrectness such as panics or wrong results.
    pub(crate) fn from_cloned_gc_ref(
        store: &mut AutoAssertNoGc<'_>,
        gc_ref: VMGcRef,
    ) -> Rooted<Self> {
        assert!(gc_ref.is_i31());
        assert!(VMGcRef::ONLY_EXTERN_REF_AND_I31);
        Rooted::new(store, gc_ref)
    }
}

unsafe impl WasmTy for Rooted<StructRef> {
    #[inline]
    fn valtype() -> ValType {
        ValType::Ref(RefType::new(false, HeapType::Any))
    }

    #[inline]
    fn compatible_with_store(&self, store: &StoreOpaque) -> bool {
        self.comes_from_same_store(store)
    }

    #[inline]
    fn dynamic_concrete_type_check(&self, _: &StoreOpaque, _: bool, _: &HeapType) -> Result<()> {
        unreachable!()
    }

    fn store(self, store: &mut AutoAssertNoGc<'_>, ptr: &mut MaybeUninit<ValRaw>) -> Result<()> {
        let gc_ref = self.inner.try_clone_gc_ref(store)?;
        let r64 = gc_ref.as_r64();
        store.gc_store_mut()?.expose_gc_ref_to_wasm(gc_ref);
        debug_assert_ne!(r64, 0);
        let anyref = u32::try_from(r64).unwrap();
        ptr.write(ValRaw::anyref(anyref));
        Ok(())
    }

    unsafe fn load(store: &mut AutoAssertNoGc<'_>, ptr: &ValRaw) -> Self {
        let raw = ptr.get_anyref();
        debug_assert_ne!(raw, 0);
        let gc_ref = VMGcRef::from_r64(raw.into())
            .expect("valid r64")
            .expect("non-null");
        let gc_ref = store.unwrap_gc_store_mut().clone_gc_ref(&gc_ref);
        StructRef::from_cloned_gc_ref(store, gc_ref)
    }
}

unsafe impl WasmTy for Option<Rooted<StructRef>> {
    #[inline]
    fn valtype() -> ValType {
        ValType::ANYREF
    }

    #[inline]
    fn compatible_with_store(&self, store: &StoreOpaque) -> bool {
        self.map_or(true, |x| x.comes_from_same_store(store))
    }

    #[inline]
    fn dynamic_concrete_type_check(&self, _: &StoreOpaque, _: bool, _: &HeapType) -> Result<()> {
        unreachable!()
    }

    #[inline]
    fn is_vmgcref_and_points_to_object(&self) -> bool {
        self.is_some()
    }

    fn store(self, store: &mut AutoAssertNoGc<'_>, ptr: &mut MaybeUninit<ValRaw>) -> Result<()> {
        match self {
            Some(r) => r.store(store, ptr),
            None => {
                ptr.write(ValRaw::anyref(0));
                Ok(())
            }
        }
    }

    unsafe fn load(store: &mut AutoAssertNoGc<'_>, ptr: &ValRaw) -> Self {
        let gc_ref = VMGcRef::from_r64(ptr.get_anyref().into()).expect("valid r64")?;
        let gc_ref = store.unwrap_gc_store_mut().clone_gc_ref(&gc_ref);
        Some(StructRef::from_cloned_gc_ref(store, gc_ref))
    }
}

unsafe impl WasmTy for ManuallyRooted<StructRef> {
    #[inline]
    fn valtype() -> ValType {
        ValType::Ref(RefType::new(false, HeapType::Any))
    }

    #[inline]
    fn compatible_with_store(&self, store: &StoreOpaque) -> bool {
        self.comes_from_same_store(store)
    }

    #[inline]
    fn dynamic_concrete_type_check(&self, _: &StoreOpaque, _: bool, _: &HeapType) -> Result<()> {
        unreachable!()
    }

    fn store(self, store: &mut AutoAssertNoGc<'_>, ptr: &mut MaybeUninit<ValRaw>) -> Result<()> {
        let gc_ref = self.inner.try_clone_gc_ref(store)?;
        let r64 = gc_ref.as_r64();
        store.gc_store_mut()?.expose_gc_ref_to_wasm(gc_ref);
        debug_assert_ne!(r64, 0);
        let anyref = u32::try_from(r64).unwrap();
        ptr.write(ValRaw::anyref(anyref));
        Ok(())
    }

    unsafe fn load(store: &mut AutoAssertNoGc<'_>, ptr: &ValRaw) -> Self {
        let raw = ptr.get_anyref();
        debug_assert_ne!(raw, 0);
        let gc_ref = VMGcRef::from_r64(raw.into())
            .expect("valid r64")
            .expect("non-null");
        let gc_ref = store.unwrap_gc_store_mut().clone_gc_ref(&gc_ref);
        RootSet::with_lifo_scope(store, |store| {
            let rooted = StructRef::from_cloned_gc_ref(store, gc_ref);
            rooted
                ._to_manually_rooted(store)
                .expect("rooted is in scope")
        })
    }
}

unsafe impl WasmTy for Option<ManuallyRooted<StructRef>> {
    #[inline]
    fn valtype() -> ValType {
        ValType::ANYREF
    }

    #[inline]
    fn compatible_with_store(&self, store: &StoreOpaque) -> bool {
        self.as_ref()
            .map_or(true, |x| x.comes_from_same_store(store))
    }

    #[inline]
    fn dynamic_concrete_type_check(&self, _: &StoreOpaque, _: bool, _: &HeapType) -> Result<()> {
        unreachable!()
    }

    #[inline]
    fn is_vmgcref_and_points_to_object(&self) -> bool {
        self.is_some()
    }

    fn store(self, store: &mut AutoAssertNoGc<'_>, ptr: &mut MaybeUninit<ValRaw>) -> Result<()> {
        match self {
            Some(r) => r.store(store, ptr),
            None => {
                ptr.write(ValRaw::anyref(0));
                Ok(())
            }
        }
    }

    unsafe fn load(store: &mut AutoAssertNoGc<'_>, ptr: &ValRaw) -> Self {
        let raw = ptr.get_anyref();
        debug_assert_ne!(raw, 0);
        let gc_ref = VMGcRef::from_r64(raw.into()).expect("valid r64")?;
        let gc_ref = store.unwrap_gc_store_mut().clone_gc_ref(&gc_ref);
        RootSet::with_lifo_scope(store, |store| {
            let rooted = StructRef::from_cloned_gc_ref(store, gc_ref);
            Some(
                rooted
                    ._to_manually_rooted(store)
                    .expect("rooted is in scope"),
            )
        })
    }
}
