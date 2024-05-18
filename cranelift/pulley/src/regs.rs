//! Pulley registers.

use core::ops::Range;

macro_rules! define_registers {
    (
        $(
            $( #[$attr:meta] )*
            pub struct $name:ident = $range:expr;
        )*
) => {
        $(
            $( #[ $attr ] )*
            #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(u8);

            impl $name {
                /// The valid register range for this register class.
                pub const RANGE: Range<u8> = $range;

                /// Construct a new register of this class.
                #[inline]
                pub fn new(index: u8) -> Option<Self> {
                    if Self::RANGE.start <= index && index < Self::RANGE.end {
                        Some(unsafe { Self::unchecked_new(index) })
                    } else {
                        None
                    }
                }

                /// Construct a new register of this class without checking that
                /// `index` is a valid register index.
                #[inline]
                pub unsafe fn unchecked_new(index: u8) -> Self {
                    debug_assert!(Self::RANGE.start <= index && index < Self::RANGE.end);
                    Self(index)
                }

                /// Get this register's index.
                #[inline]
                pub fn index(&self) -> usize {
                    usize::from(self.0)
                }
            }

            #[cfg(feature = "arbitrary")]
            impl<'a> arbitrary::Arbitrary<'a> for $name {
                fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
                    let index = u.int_in_range(Self::RANGE.start..=Self::RANGE.end - 1)?;
                    Ok(Self(index))
                }
            }

        )*
    }
}

define_registers! {
    /// TODO FITZGEN
    pub struct XReg = 0..37;

    /// TODO FITZGEN
    pub struct FReg = 0..32;

    /// TODO FITZGEN
    pub struct VReg = 0..32;
}

impl XReg {
    /// The valid special register range.
    pub const SPECIAL_RANGE: Range<u8> = 32..37;

    /// TODO FITZGEN
    pub const SP: Self = Self(32);

    /// TODO FITZGEN
    pub const LR: Self = Self(33);

    /// TODO FITZGEN
    pub const FP: Self = Self(34);

    /// TODO FITZGEN
    pub const SPILL_TMP_0: Self = Self(35);

    /// TODO FITZGEN
    pub const SPILL_TMP_1: Self = Self(36);

    /// TODO FITZGEN
    pub fn is_special(&self) -> bool {
        self.0 >= Self::SPECIAL_RANGE.start
    }
}

impl core::fmt::Display for XReg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            x if *x == Self::SP => write!(f, "sp"),
            x if *x == Self::LR => write!(f, "lr"),
            x if *x == Self::FP => write!(f, "fp"),
            x if *x == Self::SPILL_TMP_0 => write!(f, "spilltmp0"),
            x if *x == Self::SPILL_TMP_1 => write!(f, "spilltmp1"),
            Self(x) => write!(f, "x{x}"),
        }
    }
}

impl core::fmt::Display for FReg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "f{}", self.0)
    }
}

impl core::fmt::Display for VReg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "v{}", self.0)
    }
}
