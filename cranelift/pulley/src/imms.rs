//! Immediates.

/// A PC-relative offset.
///
/// This is relative to the start of this offset's containing instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PcRelOffset(i32);

impl From<i32> for PcRelOffset {
    #[inline]
    fn from(offset: i32) -> Self {
        PcRelOffset(offset)
    }
}

impl From<PcRelOffset> for i32 {
    #[inline]
    fn from(offset: PcRelOffset) -> Self {
        offset.0
    }
}
