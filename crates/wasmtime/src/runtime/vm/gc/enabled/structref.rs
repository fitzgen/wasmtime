use crate::{
    runtime::vm::{GcHeap, GcStore, VMGcRef},
    Engine, V128,
};
use core::fmt;
use wasmtime_environ::{VMGcKind, WasmCompositeType};

/// A `VMGcRef` that we know points to a `struct`.
///
/// Create a `VMStructRef` via `VMGcRef::into_structref` and
/// `VMGcRef::as_structref`, or their untyped equivalents
/// `VMGcRef::into_structref_unchecked` and `VMGcRef::as_structref_unchecked`.
///
/// Note: This is not a `TypedGcRef<_>` because each collector can have a
/// different concrete representation of `structref` that they allocate inside
/// their heaps.
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VMStructRef(VMGcRef);

impl fmt::Pointer for VMStructRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.0, f)
    }
}

impl From<VMStructRef> for VMGcRef {
    #[inline]
    fn from(x: VMStructRef) -> Self {
        x.0
    }
}

impl VMGcRef {
    /// TODO FITZGEN
    pub fn is_structref(&self, engine: &Engine, gc_heap: &impl GcHeap) -> bool {
        if self.is_i31() {
            return false;
        }

        let header = gc_heap.header(&self);
        match header.ty().and_then(|ty| engine.signatures().borrow(ty)) {
            Some(ty) => {
                debug_assert_eq!(header.kind(), VMGcKind::AnyRef);
                match &ty.composite_type {
                    WasmCompositeType::Struct(_) => true,
                    WasmCompositeType::Array(_) | WasmCompositeType::Func(_) => false,
                }
            }
            None => {
                debug_assert_eq!(header.kind(), VMGcKind::ExternRef);
                false
            }
        }
    }

    /// Create a new `VMStructRef` from the given `gc_ref`.
    ///
    /// If this is not a GC reference to an `structref`, `Err(self)` is
    /// returned.
    pub fn into_structref(
        self,
        engine: &Engine,
        gc_heap: &impl GcHeap,
    ) -> Result<VMStructRef, VMGcRef> {
        if self.is_structref(engine, gc_heap) {
            Ok(self.into_structref_unchecked())
        } else {
            Err(self)
        }
    }

    /// Create a new `VMStructRef` from `self` without actually checking that
    /// `self` is an `structref`.
    ///
    /// This method does not check that `self` is actually an `structref`, but
    /// it should be. Failure to uphold this invariant is memory safe but will
    /// result in general incorrectness down the line such as panics or wrong
    /// results.
    #[inline]
    pub fn into_structref_unchecked(self) -> VMStructRef {
        debug_assert!(!self.is_i31());
        VMStructRef(self)
    }

    /// Get this GC reference as an `structref` reference, if it actually is an
    /// `structref` reference.
    pub fn as_structref(&self, engine: &Engine, gc_heap: &impl GcHeap) -> Option<&VMStructRef> {
        if self.is_structref(engine, gc_heap) {
            Some(self.as_structref_unchecked())
        } else {
            None
        }
    }

    /// Get this GC reference as an `structref` reference without checking if it
    /// actually is an `structref` reference.
    ///
    /// Calling this method on a non-`structref` reference is memory safe, but
    /// will lead to general incorrectness like panics and wrong results.
    pub fn as_structref_unchecked(&self) -> &VMStructRef {
        debug_assert!(!self.is_i31());
        let ptr = self as *const VMGcRef;
        let ret = unsafe { &*ptr.cast() };
        assert!(matches!(ret, VMStructRef(VMGcRef { .. })));
        ret
    }
}

impl VMStructRef {
    /// Get the underlying `VMGcRef`.
    pub fn as_gc_ref(&self) -> &VMGcRef {
        &self.0
    }

    /// Clone this `VMStructRef`, running any GC barriers as necessary.
    pub fn clone(&self, gc_store: &mut GcStore) -> Self {
        Self(gc_store.clone_gc_ref(&self.0))
    }

    /// Explicitly drop this `structref`, running GC drop barriers as necessary.
    pub fn drop(self, gc_store: &mut GcStore) {
        gc_store.drop_gc_ref(self.0);
    }

    /// Copy this `VMStructRef` without running the GC's clone barriers.
    ///
    /// Prefer calling `clone(&mut GcStore)` instead! This is mostly an internal
    /// escape hatch for collector implementations.
    ///
    /// Failure to run GC barriers when they would otherwise be necessary can
    /// lead to leaks, panics, and wrong results. It cannot lead to memory
    /// unsafety, however.
    pub fn unchecked_copy(&self) -> Self {
        Self(self.0.unchecked_copy())
    }
}

/// TODO FITZGEN
pub unsafe trait PodValType: Copy {
    fn read_le(le_bytes: &[u8]) -> Self;
    unsafe fn write_le(&self, into: *mut u8);
}

unsafe impl PodValType for u8 {
    fn read_le(le_bytes: &[u8]) -> Self {
        u8::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for u16 {
    fn read_le(le_bytes: &[u8]) -> Self {
        u16::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for u32 {
    fn read_le(le_bytes: &[u8]) -> Self {
        u32::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for u64 {
    fn read_le(le_bytes: &[u8]) -> Self {
        u64::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for usize {
    fn read_le(le_bytes: &[u8]) -> Self {
        usize::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for i8 {
    fn read_le(le_bytes: &[u8]) -> Self {
        i8::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for i16 {
    fn read_le(le_bytes: &[u8]) -> Self {
        i16::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for i32 {
    fn read_le(le_bytes: &[u8]) -> Self {
        i32::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for i64 {
    fn read_le(le_bytes: &[u8]) -> Self {
        i64::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}
unsafe impl PodValType for isize {
    fn read_le(le_bytes: &[u8]) -> Self {
        isize::from_le_bytes(le_bytes.try_into().unwrap())
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}

unsafe impl PodValType for V128 {
    fn read_le(le_bytes: &[u8]) -> Self {
        let u128 = u128::from_le_bytes(le_bytes.try_into().unwrap());
        u128.into()
    }
    unsafe fn write_le(&self, into: *mut u8) {
        let le_bytes = self.as_u128().to_le_bytes();
        core::ptr::copy_nonoverlapping(le_bytes.as_ptr(), into, le_bytes.len());
    }
}

/// TODO FITZGEN
pub struct VMStructDataMut<'a> {
    data: &'a mut [u8],
}

impl<'a> VMStructDataMut<'a> {
    pub fn new(data: &'a mut [u8]) -> Self {
        Self { data }
    }

    /// Read a POD field out of this struct.
    pub fn read_pod<T: PodValType>(&self, offset: u32) -> T {
        let offset = usize::try_from(offset).unwrap();
        let end = offset.checked_add(core::mem::size_of::<T>()).unwrap();
        let bytes = self.data.get(offset..end).expect("out of bounds field");
        T::read_le(bytes)
    }

    /// Read a POD field out of this struct.
    pub fn write_pod<T: PodValType>(&mut self, offset: u32, val: T) {
        let offset = usize::try_from(offset).unwrap();
        let end = offset.checked_add(core::mem::size_of::<T>()).unwrap();
        let into = self.data.get_mut(offset..end).expect("out of bounds field");
        unsafe { val.write_le(into.as_mut_ptr()) };
    }
}
