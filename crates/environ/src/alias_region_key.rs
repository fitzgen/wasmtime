use crate::{DefinedGlobalIndex, DefinedMemoryIndex, DefinedTableIndex, StaticModuleIndex};
use core::fmt;

/// A key that uniquely identifies an alias region across an entire compilation.
///
/// This is used to assign stable `user_id`s to `AliasRegionData` entries so
/// that alias regions can be deduplicated during inlining.
///
/// The key encodes into a single `u32` with the following layout:
/// `[ kind: 4 bits | data: 28 bits ]`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AliasRegionKey {
    /// A `VMContext` field access.
    ///
    /// Raw encoding: `[ kind: 0b0000 | offset: u28 ]`
    VMContext {
        /// The offset of the `VMContext` field being accessed (or the base
        /// of the array for `VMContext` array fields).
        offset: u32,
    },

    /// A `VMStoreContext` field access.
    ///
    /// Raw encoding: `[ kind: 0b0001 | offset: u28 ]`
    VMStoreContext {
        /// The offset of the `VMStoreContext` field being accessed.
        offset: u32,
    },

    /// An imported memory access (shared across all imported memories).
    ///
    /// Raw encoding: `[ kind: 0b0010 | _unused: u28 ]`
    ImportedMemory,

    /// A defined memory access.
    ///
    /// Raw encoding: `[ kind: 0b0011 | module: u8 | index: u20 ]`
    DefinedMemory {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined memory index within the module.
        index: DefinedMemoryIndex,
    },

    /// An imported table access (shared across all imported tables).
    ///
    /// Raw encoding: `[ kind: 0b0100 | _unused: u28 ]`
    ImportedTable,

    /// A defined table access.
    ///
    /// Raw encoding: `[ kind: 0b0101 | module: u8 | index: u20 ]`
    DefinedTable {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined table index within the module.
        index: DefinedTableIndex,
    },

    /// An imported global access (shared across all imported globals).
    ///
    /// Raw encoding: `[ kind: 0b0110 | _unused: u28 ]`
    ImportedGlobal,

    /// A defined global access.
    ///
    /// Raw encoding: `[ kind: 0b0111 | module: u8 | index: u20 ]`
    DefinedGlobal {
        /// The static module index.
        module: StaticModuleIndex,
        /// The defined global index within the module.
        index: DefinedGlobalIndex,
    },

    /// A GC heap access.
    ///
    /// Raw encoding: `[ kind: 0b1000 | _unused: u28 ]`
    GcHeap,
}

impl AliasRegionKey {
    const KIND_BITS: u32 = 4;
    const KIND_OFFSET: u32 = 32 - Self::KIND_BITS;
    const KIND_MASK: u32 = ((1 << Self::KIND_BITS) - 1) << Self::KIND_OFFSET;

    const OFFSET_MASK: u32 = !Self::KIND_MASK;

    const MODULE_BITS: u32 = 8;
    const MODULE_OFFSET: u32 = Self::KIND_OFFSET - Self::MODULE_BITS;
    const MODULE_MASK: u32 = ((1 << Self::MODULE_BITS) - 1) << Self::MODULE_OFFSET;

    const INDEX_MASK: u32 = !Self::KIND_MASK & !Self::MODULE_MASK;

    const fn new_kind(kind: u32) -> u32 {
        assert!(kind < (1 << Self::KIND_BITS));
        kind << Self::KIND_OFFSET
    }

    const VM_CONTEXT_KIND: u32 = Self::new_kind(0b0000);
    const VM_STORE_CONTEXT_KIND: u32 = Self::new_kind(0b0001);
    const IMPORTED_MEMORY_KIND: u32 = Self::new_kind(0b0010);
    const DEFINED_MEMORY_KIND: u32 = Self::new_kind(0b0011);
    const IMPORTED_TABLE_KIND: u32 = Self::new_kind(0b0100);
    const DEFINED_TABLE_KIND: u32 = Self::new_kind(0b0101);
    const IMPORTED_GLOBAL_KIND: u32 = Self::new_kind(0b0110);
    const DEFINED_GLOBAL_KIND: u32 = Self::new_kind(0b0111);
    const GC_HEAP_KIND: u32 = Self::new_kind(0b1000);

    /// Encode this key into a raw `u32` suitable for use as an
    /// `AliasRegionData::user_id`.
    pub fn into_raw(self) -> u32 {
        match self {
            AliasRegionKey::VMContext { offset } => {
                debug_assert_eq!(offset & Self::KIND_MASK, 0);
                Self::VM_CONTEXT_KIND | (offset & Self::OFFSET_MASK)
            }
            AliasRegionKey::VMStoreContext { offset } => {
                debug_assert_eq!(offset & Self::KIND_MASK, 0);
                Self::VM_STORE_CONTEXT_KIND | (offset & Self::OFFSET_MASK)
            }
            AliasRegionKey::ImportedMemory => Self::IMPORTED_MEMORY_KIND,
            AliasRegionKey::DefinedMemory { module, index } => {
                debug_assert_eq!(
                    module.as_u32() & !Self::MODULE_MASK >> Self::MODULE_OFFSET,
                    0
                );
                debug_assert_eq!(index.as_u32() & !Self::INDEX_MASK, 0);
                Self::DEFINED_MEMORY_KIND
                    | (module.as_u32() << Self::MODULE_OFFSET)
                    | index.as_u32()
            }
            AliasRegionKey::ImportedTable => Self::IMPORTED_TABLE_KIND,
            AliasRegionKey::DefinedTable { module, index } => {
                debug_assert_eq!(
                    module.as_u32() & !Self::MODULE_MASK >> Self::MODULE_OFFSET,
                    0
                );
                debug_assert_eq!(index.as_u32() & !Self::INDEX_MASK, 0);
                Self::DEFINED_TABLE_KIND | (module.as_u32() << Self::MODULE_OFFSET) | index.as_u32()
            }
            AliasRegionKey::ImportedGlobal => Self::IMPORTED_GLOBAL_KIND,
            AliasRegionKey::DefinedGlobal { module, index } => {
                debug_assert_eq!(
                    module.as_u32() & !Self::MODULE_MASK >> Self::MODULE_OFFSET,
                    0
                );
                debug_assert_eq!(index.as_u32() & !Self::INDEX_MASK, 0);
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
            AliasRegionKey::VMContext { offset } => write!(f, "VMContext+{offset:#x}"),
            AliasRegionKey::VMStoreContext { offset } => write!(f, "VMStoreContext+{offset:#x}"),
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
        assert_eq!(AliasRegionKey::ImportedMemory.into_raw(), 0x20000000);
        assert_eq!(AliasRegionKey::ImportedTable.into_raw(), 0x40000000);
        assert_eq!(AliasRegionKey::ImportedGlobal.into_raw(), 0x60000000);
        assert_eq!(AliasRegionKey::GcHeap.into_raw(), 0x80000000);
    }

    #[test]
    fn vmcontext_offset() {
        let key = AliasRegionKey::VMContext { offset: 0x100 };
        assert_eq!(key.into_raw(), 0x00000100);
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
            AliasRegionKey::VMContext { offset: 0 },
            AliasRegionKey::VMStoreContext { offset: 0 },
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
}
