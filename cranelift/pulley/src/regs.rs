//! Pulley registers.

use core::ops::Range;

macro_rules! define_registers {
    (
        $(
            $( #[$attr:meta] )*
            pub struct $name:ident ;
        )*
) => {
        $(
            $( #[ $attr ] )*
            #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(u8);

            impl $name {
                /// The valid register range for this register class.
                pub const RANGE: Range<u8> = 0..32;

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
        )*
    }
}

define_registers! {
    /// TODO FITZGEN
    pub struct XReg;

    /// TODO FITZGEN
    pub struct FReg;

    /// TODO FITZGEN
    pub struct VReg;
}

impl XReg {
    /// The valid special register range.
    pub const SPECIAL_RANGE: Range<u8> = 32..37;

    /// Construct a new special register of this class.
    #[inline]
    pub fn special(index: u8) -> Option<Self> {
        if Self::SPECIAL_RANGE.start <= index && index < Self::SPECIAL_RANGE.end {
            Some(unsafe { Self::unchecked_special(index) })
        } else {
            None
        }
    }
    /// Construct a new special register of this class without checking that
    /// `index` is a valid special register index.
    #[inline]
    pub unsafe fn unchecked_special(index: u8) -> Self {
        debug_assert!(Self::SPECIAL_RANGE.start <= index && index < Self::SPECIAL_RANGE.end);
        Self(index)
    }
}

impl core::fmt::Display for XReg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.0 {
            32 => write!(f, "sp"),
            33 => write!(f, "lr"),
            34 => write!(f, "fp"),
            35 => write!(f, "spilltmp0"),
            36 => write!(f, "spilltmp1"),
            x => write!(f, "x{x}"),
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for XReg {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        if u.arbitrary()? {
            let index = u.int_in_range(XReg::RANGE.start..=XReg::RANGE.end - 1)?;
            Ok(XReg::new(index).unwrap())
        } else {
            let index = u.int_in_range(XReg::SPECIAL_RANGE.start..=XReg::SPECIAL_RANGE.end - 1)?;
            Ok(XReg::special(index).unwrap())
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for FReg {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let index = u.int_in_range(FReg::RANGE.start..=FReg::RANGE.end - 1)?;
        Ok(FReg::new(index).unwrap())
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for VReg {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let index = u.int_in_range(VReg::RANGE.start..=VReg::RANGE.end - 1)?;
        Ok(VReg::new(index).unwrap())
    }
}
