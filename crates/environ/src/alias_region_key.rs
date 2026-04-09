use crate::{DefinedGlobalIndex, DefinedMemoryIndex, DefinedTableIndex, StaticModuleIndex};
use core::fmt;

/// Identifies the type of VM struct being accessed through a pointer.
///
/// Each variant corresponds to a distinct C-repr struct type whose size and
/// field offsets are computed in `VMOffsets` or `VMComponentOffsets`.
///
/// Uses 5 bits in the bitpacked encoding, supporting up to 32 types.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(u8)]
pub enum VmctxType {
    /// `VMContext` — the per-instance context.
    VMContext = 0,
    /// `VMStoreContext` — store-level state (fuel, epoch, GC heap, etc.).
    VMStoreContext = 1,
    /// `VMFuncRef` — a function reference with type info and call targets.
    VMFuncRef = 2,
    /// `VMGlobalDefinition` — storage for a single global variable.
    VMGlobalDefinition = 3,
    /// `VMTagDefinition` — metadata for an exception tag.
    VMTagDefinition = 4,
    /// `VMMemoryDefinition` — base pointer and size of a linear memory.
    VMMemoryDefinition = 5,
    /// `VMTableDefinition` — base pointer and element count of a table.
    VMTableDefinition = 6,
    /// `VMFunctionImport` — an imported function (body + vmctx pair).
    VMFunctionImport = 7,
    /// `VMTableImport` — a pointer to the imported table's definition.
    VMTableImport = 8,
    /// `VMMemoryImport` — a pointer to the imported memory's definition.
    VMMemoryImport = 9,
    /// `VMGlobalImport` — a pointer to the imported global's definition.
    VMGlobalImport = 10,
    /// `VMTagImport` — a pointer to the imported tag's definition.
    VMTagImport = 11,
    /// `VMArrayCallHostFuncContext` — context for host-to-array call trampolines.
    VMArrayCallHostFuncContext = 12,
    /// `VMStackLimits` — stack limit pointers for stack overflow checks.
    VMStackLimits = 13,
    /// `VMCommonStackInformation` — shared stack metadata.
    VMCommonStackInformation = 14,
    /// `VMGcHeader` — common header for all GC-managed objects.
    VMGcHeader = 15,
    /// `VMDrcHeader` — DRC-specific GC object header.
    VMDrcHeader = 16,
    /// `VMContObj` — continuation object.
    VMContObj = 17,
    /// `VMContRef` — continuation reference.
    VMContRef = 18,
    /// `VMComponentContext` — per-component-instance context.
    VMComponentContext = 19,
    /// `VMLowering` — a component model lowering (callee + data pair).
    VMLowering = 20,
}

impl VmctxType {
    const MAX: u32 = 20;
}

/// A key that uniquely identifies an alias region across an entire compilation.
///
/// This is used to assign stable `user_id`s to `AliasRegionData` entries so
/// that alias regions can be deduplicated during inlining.
///
/// The key encodes into a single `u32` with the following layout:
/// `[ kind: 4 bits | data: 28 bits ]`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AliasRegionKey {
    /// A VM struct field access (VMContext, VMStoreContext, VMFuncRef, etc.).
    ///
    /// Raw encoding: `[ kind: 0b0000 | vmctx_type: u5 | offset: u23 ]`
    Vmctx {
        /// The type of VM struct being accessed.
        ty: VmctxType,
        /// The byte offset of the field within the struct.
        offset: u32,
    },

    /// An imported memory access (shared across all imported memories).
    ///
    /// Raw encoding: `[ kind: 0b0001 | _unused: u28 ]`
    ImportedMemory,

    /// A defined memory access.
    ///
    /// Raw encoding: `[ kind: 0b0010 | module: u8 | index: u20 ]`
    DefinedMemory {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined memory index within the module.
        index: DefinedMemoryIndex,
    },

    /// An imported table access (shared across all imported tables).
    ///
    /// Raw encoding: `[ kind: 0b0011 | _unused: u28 ]`
    ImportedTable,

    /// A defined table access.
    ///
    /// Raw encoding: `[ kind: 0b0100 | module: u8 | index: u20 ]`
    DefinedTable {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined table index within the module.
        index: DefinedTableIndex,
    },

    /// An imported global access (shared across all imported globals).
    ///
    /// Raw encoding: `[ kind: 0b0101 | _unused: u28 ]`
    ImportedGlobal,

    /// A defined global access.
    ///
    /// Raw encoding: `[ kind: 0b0110 | module: u8 | index: u20 ]`
    DefinedGlobal {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined global index within the module.
        index: DefinedGlobalIndex,
    },

    /// A GC heap access.
    ///
    /// Raw encoding: `[ kind: 0b0111 | _unused: u28 ]`
    GcHeap,
}

impl AliasRegionKey {
    const KIND_BITS: u32 = 4;
    const KIND_OFFSET: u32 = 32 - Self::KIND_BITS;
    const KIND_MASK: u32 = ((1 << Self::KIND_BITS) - 1) << Self::KIND_OFFSET;

    // Vmctx data layout within the 28 data bits: [vmctx_type:5 | offset:23]
    const VMCTX_TYPE_BITS: u32 = 5;
    const VMCTX_TYPE_OFFSET: u32 = Self::KIND_OFFSET - Self::VMCTX_TYPE_BITS;
    const VMCTX_OFFSET_MASK: u32 = (1 << Self::VMCTX_TYPE_OFFSET) - 1;

    // Defined entity data layout: [module:8 | index:20]
    const MODULE_BITS: u32 = 8;
    const MODULE_OFFSET: u32 = Self::KIND_OFFSET - Self::MODULE_BITS;
    const MODULE_MASK: u32 = ((1 << Self::MODULE_BITS) - 1) << Self::MODULE_OFFSET;
    const INDEX_MASK: u32 = !Self::KIND_MASK & !Self::MODULE_MASK;

    const fn new_kind(kind: u32) -> u32 {
        assert!(kind < (1 << Self::KIND_BITS));
        kind << Self::KIND_OFFSET
    }

    const VMCTX_KIND: u32 = Self::new_kind(0b0000);
    const IMPORTED_MEMORY_KIND: u32 = Self::new_kind(0b0001);
    const DEFINED_MEMORY_KIND: u32 = Self::new_kind(0b0010);
    const IMPORTED_TABLE_KIND: u32 = Self::new_kind(0b0011);
    const DEFINED_TABLE_KIND: u32 = Self::new_kind(0b0100);
    const IMPORTED_GLOBAL_KIND: u32 = Self::new_kind(0b0101);
    const DEFINED_GLOBAL_KIND: u32 = Self::new_kind(0b0110);
    const GC_HEAP_KIND: u32 = Self::new_kind(0b0111);

    /// The maximum byte offset that can be encoded in a `Vmctx` key.
    pub const MAX_VMCTX_OFFSET: u32 = Self::VMCTX_OFFSET_MASK;

    /// Encode this key into a raw `u32` suitable for use as an
    /// `AliasRegionData::user_id`.
    pub fn into_raw(self) -> u32 {
        match self {
            AliasRegionKey::Vmctx { ty, offset } => {
                let ty_bits = ty as u32;
                debug_assert!(ty_bits <= VmctxType::MAX);
                debug_assert!(
                    offset <= Self::VMCTX_OFFSET_MASK,
                    "vmctx offset {offset:#x} exceeds max {:#x}",
                    Self::VMCTX_OFFSET_MASK,
                );
                Self::VMCTX_KIND | (ty_bits << Self::VMCTX_TYPE_OFFSET) | offset
            }
            AliasRegionKey::ImportedMemory => Self::IMPORTED_MEMORY_KIND,
            AliasRegionKey::DefinedMemory { module, index } => {
                debug_assert!(module.as_u32() < (1 << Self::MODULE_BITS));
                debug_assert!(index.as_u32() <= Self::INDEX_MASK);
                Self::DEFINED_MEMORY_KIND
                    | (module.as_u32() << Self::MODULE_OFFSET)
                    | index.as_u32()
            }
            AliasRegionKey::ImportedTable => Self::IMPORTED_TABLE_KIND,
            AliasRegionKey::DefinedTable { module, index } => {
                debug_assert!(module.as_u32() < (1 << Self::MODULE_BITS));
                debug_assert!(index.as_u32() <= Self::INDEX_MASK);
                Self::DEFINED_TABLE_KIND | (module.as_u32() << Self::MODULE_OFFSET) | index.as_u32()
            }
            AliasRegionKey::ImportedGlobal => Self::IMPORTED_GLOBAL_KIND,
            AliasRegionKey::DefinedGlobal { module, index } => {
                debug_assert!(module.as_u32() < (1 << Self::MODULE_BITS));
                debug_assert!(index.as_u32() <= Self::INDEX_MASK);
                Self::DEFINED_GLOBAL_KIND
                    | (module.as_u32() << Self::MODULE_OFFSET)
                    | index.as_u32()
            }
            AliasRegionKey::GcHeap => Self::GC_HEAP_KIND,
        }
    }
}

impl fmt::Debug for AliasRegionKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AliasRegionKey::Vmctx { ty, offset } => write!(f, "{ty:?}+{offset:#x}"),
            AliasRegionKey::ImportedMemory => write!(f, "ImportedMemory"),
            AliasRegionKey::DefinedMemory { module, index } => {
                write!(f, "DefinedMemory({module:?}, {index:?})")
            }
            AliasRegionKey::ImportedTable => write!(f, "ImportedTable"),
            AliasRegionKey::DefinedTable { module, index } => {
                write!(f, "DefinedTable({module:?}, {index:?})")
            }
            AliasRegionKey::ImportedGlobal => write!(f, "ImportedGlobal"),
            AliasRegionKey::DefinedGlobal { module, index } => {
                write!(f, "DefinedGlobal({module:?}, {index:?})")
            }
            AliasRegionKey::GcHeap => write!(f, "GcHeap"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_simple_kinds() {
        assert_eq!(AliasRegionKey::ImportedMemory.into_raw(), 0x10000000);
        assert_eq!(AliasRegionKey::ImportedTable.into_raw(), 0x30000000);
        assert_eq!(AliasRegionKey::ImportedGlobal.into_raw(), 0x50000000);
        assert_eq!(AliasRegionKey::GcHeap.into_raw(), 0x70000000);
    }

    #[test]
    fn vmctx_offset() {
        let key = AliasRegionKey::Vmctx {
            ty: VmctxType::VMContext,
            offset: 0x100,
        };
        assert_eq!(key.into_raw(), 0x00000100);
    }

    #[test]
    fn vmctx_store_context() {
        let key = AliasRegionKey::Vmctx {
            ty: VmctxType::VMStoreContext,
            offset: 0x40,
        };
        let raw = key.into_raw();
        // kind=0b0000, vmctx_type=1 shifted left by 23 = 0x00800000
        assert_eq!(raw, 0x00800040);
    }

    #[test]
    fn vmctx_max_offset() {
        let key = AliasRegionKey::Vmctx {
            ty: VmctxType::VMContext,
            offset: AliasRegionKey::MAX_VMCTX_OFFSET,
        };
        let raw = key.into_raw();
        assert_eq!(
            raw & AliasRegionKey::VMCTX_OFFSET_MASK,
            AliasRegionKey::MAX_VMCTX_OFFSET
        );
    }

    #[test]
    fn defined_memory_encoding() {
        let key = AliasRegionKey::DefinedMemory {
            module: StaticModuleIndex::from_u32(1),
            index: DefinedMemoryIndex::from_u32(2),
        };
        let raw = key.into_raw();
        assert_eq!(
            raw & AliasRegionKey::KIND_MASK,
            AliasRegionKey::DEFINED_MEMORY_KIND
        );
        assert_eq!(
            (raw & AliasRegionKey::MODULE_MASK) >> AliasRegionKey::MODULE_OFFSET,
            1
        );
        assert_eq!(raw & AliasRegionKey::INDEX_MASK, 2);
    }

    #[test]
    fn unique_kinds() {
        // All simple kinds produce different raw values.
        let keys = [
            AliasRegionKey::Vmctx {
                ty: VmctxType::VMContext,
                offset: 0,
            },
            AliasRegionKey::Vmctx {
                ty: VmctxType::VMStoreContext,
                offset: 0,
            },
            AliasRegionKey::ImportedMemory,
            AliasRegionKey::ImportedTable,
            AliasRegionKey::ImportedGlobal,
            AliasRegionKey::GcHeap,
        ];
        for (i, a) in keys.iter().enumerate() {
            for (j, b) in keys.iter().enumerate() {
                if i != j {
                    assert_ne!(a.into_raw(), b.into_raw(), "{a:?} vs {b:?}");
                }
            }
        }
    }

    #[test]
    fn all_vmctx_types_unique() {
        // All VmctxType variants with the same offset produce unique raw values.
        let types = [
            VmctxType::VMContext,
            VmctxType::VMStoreContext,
            VmctxType::VMFuncRef,
            VmctxType::VMGlobalDefinition,
            VmctxType::VMTagDefinition,
            VmctxType::VMMemoryDefinition,
            VmctxType::VMTableDefinition,
            VmctxType::VMFunctionImport,
            VmctxType::VMTableImport,
            VmctxType::VMMemoryImport,
            VmctxType::VMGlobalImport,
            VmctxType::VMTagImport,
            VmctxType::VMArrayCallHostFuncContext,
            VmctxType::VMStackLimits,
            VmctxType::VMCommonStackInformation,
            VmctxType::VMGcHeader,
            VmctxType::VMDrcHeader,
            VmctxType::VMContObj,
            VmctxType::VMContRef,
            VmctxType::VMComponentContext,
            VmctxType::VMLowering,
        ];
        for (i, a) in types.iter().enumerate() {
            for (j, b) in types.iter().enumerate() {
                if i != j {
                    let ka = AliasRegionKey::Vmctx { ty: *a, offset: 0 };
                    let kb = AliasRegionKey::Vmctx { ty: *b, offset: 0 };
                    assert_ne!(ka.into_raw(), kb.into_raw(), "{a:?} vs {b:?}");
                }
            }
        }
    }
}
