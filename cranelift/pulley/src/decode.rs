//! Decoding support for pulley bytecode.

use crate::opcode::*;
use crate::regs::*;

/// TODO FITZGEN
pub type Result<T, E = DecodingError> = core::result::Result<T, E>;

/// TODO FITZGEN
pub enum DecodingError {
    /// TODO FITZGEN
    UnexpectedEof {
        /// TODO FITZGEN
        position: usize,
    },
    /// TODO FITZGEN
    InvalidOpcode {
        /// TODO FITZGEN
        position: usize,
        /// TODO FITZGEN
        code: u8,
    },
    /// TODO FITZGEN
    InvalidExtendedOpcode {
        /// TODO FITZGEN
        position: usize,
        /// TODO FITZGEN
        code: u16,
    },
    /// TODO FITZGEN
    InvalidReg {
        /// TODO FITZGEN
        position: usize,
        /// TODO FITZGEN
        reg: u8,
    },
}

impl core::fmt::Debug for DecodingError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Display for DecodingError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnexpectedEof { position } => {
                write!(f, "unexpected end-of-file at bytecode offset {position:#x}")
            }
            Self::InvalidOpcode { position, code } => {
                write!(
                    f,
                    "found invalid opcode {code:#x} at bytecode offset {position:#x}"
                )
            }
            Self::InvalidExtendedOpcode { position, code } => {
                write!(
                    f,
                    "found invalid opcode {code:#x} at bytecode offset {position:#x}"
                )
            }
            Self::InvalidReg { position, reg } => {
                write!(
                    f,
                    "found invalid register {reg:#x} at bytecode offset {position:#x}"
                )
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for DecodingError {}

trait DecodeRawMethods {
    type Error;
    fn unexpected_eof(position: usize) -> Self::Error;
    fn invalid_opcode(position: usize, code: u8) -> Self::Error;
    fn invalid_extended_opcode(position: usize, code: u16) -> Self::Error;
    fn invalid_reg(position: usize, reg: u8) -> Self::Error;
}

struct SafeDecodeRawMethods;
impl DecodeRawMethods for SafeDecodeRawMethods {
    type Error = DecodingError;
    fn unexpected_eof(position: usize) -> Self::Error {
        DecodingError::UnexpectedEof { position }
    }
    fn invalid_opcode(position: usize, code: u8) -> Self::Error {
        DecodingError::InvalidOpcode { position, code }
    }
    fn invalid_extended_opcode(position: usize, code: u16) -> Self::Error {
        DecodingError::InvalidExtendedOpcode { position, code }
    }
    fn invalid_reg(position: usize, reg: u8) -> Self::Error {
        DecodingError::InvalidReg { position, reg }
    }
}

enum Uninhabited {}

struct UnsafeDecodeRawMethods;
impl DecodeRawMethods for UnsafeDecodeRawMethods {
    type Error = Uninhabited;
    fn unexpected_eof(_position: usize) -> Self::Error {
        unsafe { crate::unreachable_unchecked() }
    }
    fn invalid_opcode(_position: usize, _code: u8) -> Self::Error {
        unsafe { crate::unreachable_unchecked() }
    }
    fn invalid_extended_opcode(_position: usize, _code: u16) -> Self::Error {
        unsafe { crate::unreachable_unchecked() }
    }
    fn invalid_reg(_position: usize, _reg: u8) -> Self::Error {
        unsafe { crate::unreachable_unchecked() }
    }
}

trait Decode: Sized {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods;
}

impl Decode for u8 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = bytecode
            .get(0)
            .copied()
            .ok_or_else(|| T::unexpected_eof(position))?;
        Ok((1, byte))
    }
}

impl Decode for u16 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<u16>();
        let (u16_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, u16::from_le_bytes(u16_bytes.try_into().unwrap())))
    }
}

impl Decode for u32 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<u32>();
        let (u32_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, u32::from_le_bytes(u32_bytes.try_into().unwrap())))
    }
}

impl Decode for u64 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<u64>();
        let (u64_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, u64::from_le_bytes(u64_bytes.try_into().unwrap())))
    }
}

impl Decode for i8 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = bytecode
            .get(0)
            .copied()
            .ok_or_else(|| T::unexpected_eof(position))?;
        Ok((1, byte as i8))
    }
}

impl Decode for i16 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<i16>();
        let (u16_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, i16::from_le_bytes(u16_bytes.try_into().unwrap())))
    }
}

impl Decode for i32 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<i32>();
        let (u32_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, i32::from_le_bytes(u32_bytes.try_into().unwrap())))
    }
}

impl Decode for i64 {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let size = core::mem::size_of::<i64>();
        let (u64_bytes, _) = bytecode
            .split_at_checked(size)
            .ok_or_else(|| T::unexpected_eof(position + bytecode.len()))?;
        Ok((size, i64::from_le_bytes(u64_bytes.try_into().unwrap())))
    }
}

impl Decode for XReg {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let (size, byte) = u8::decode::<T>(position, bytecode)?;
        let reg = XReg::new(byte).ok_or_else(|| T::invalid_reg(position + size, byte))?;
        Ok((size, reg))
    }
}

impl Decode for FReg {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let (size, byte) = u8::decode::<T>(position, bytecode)?;
        let reg = FReg::new(byte).ok_or_else(|| T::invalid_reg(position + size, byte))?;
        Ok((size, reg))
    }
}

impl Decode for VReg {
    fn decode<T>(position: usize, bytecode: &[u8]) -> Result<(usize, Self), T::Error>
    where
        T: DecodeRawMethods,
    {
        let (size, byte) = u8::decode::<T>(position, bytecode)?;
        let reg = VReg::new(byte).ok_or_else(|| T::invalid_reg(position + size, byte))?;
        Ok((size, reg))
    }
}

/// TODO FITZGEN
pub struct Decoder<V> {
    visitor: V,
    position: usize,
}

impl<V> Decoder<V>
where
    V: OpVisitor + ExtendedOpVisitor,
{
    /// TODO FITZGEN
    pub fn new(visitor: V) -> Self {
        Self {
            visitor,
            position: 0,
        }
    }

    /// TODO FITZGEN
    pub fn position(&self) -> usize {
        self.position
    }

    /// TODO FITZGEN
    pub fn decode_one(&mut self, bytecode: &[u8]) -> Result<V::Return> {
        unsafe { self.decode_raw::<SafeDecodeRawMethods>(bytecode) }
    }

    /// TODO FITZGEN
    pub fn decode_one_extended(&mut self, bytecode: &[u8]) -> Result<V::Return> {
        unsafe { self.decode_extended_raw::<SafeDecodeRawMethods>(bytecode) }
    }

    /// TODO FITZGEN
    pub fn decode_all(visitor: V, mut bytecode: &[u8]) -> Result<V> {
        let mut decoder = Decoder::new(visitor);

        while !bytecode.is_empty() {
            let is_extended_op = match bytecode.get(0).copied().and_then(Opcode::new) {
                Some(Opcode::ExtendedOp) => true,
                _ => false,
            };

            let start = decoder.position;
            decoder.decode_one(bytecode)?;
            let size = decoder.position - start;
            bytecode = &bytecode[size..];

            if is_extended_op {
                let start = decoder.position;
                decoder.decode_one_extended(bytecode)?;
                let size = decoder.position - start;
                bytecode = &bytecode[size..];
            }
        }

        Ok(decoder.into_visitor())
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_decode_one(&mut self, bytecode: &[u8]) -> V::Return {
        match self.decode_raw::<UnsafeDecodeRawMethods>(bytecode) {
            Ok(x) => x,
            Err(uninhabited) => match uninhabited {},
        }
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_decode_one_extended(&mut self, bytecode: &[u8]) -> V::Return {
        match self.decode_extended_raw::<UnsafeDecodeRawMethods>(bytecode) {
            Ok(x) => x,
            Err(uninhabited) => match uninhabited {},
        }
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_decode_all(visitor: V, mut bytecode: &[u8]) -> V {
        let mut decoder = Decoder::new(visitor);

        while !bytecode.is_empty() {
            let is_extended_op = match bytecode.get(0).copied().and_then(Opcode::new) {
                Some(Opcode::ExtendedOp) => true,
                _ => false,
            };

            let start = decoder.position;
            decoder.unchecked_decode_one(bytecode);
            let size = decoder.position - start;
            bytecode = &bytecode[size..];

            if is_extended_op {
                let start = decoder.position;
                decoder.unchecked_decode_one_extended(bytecode);
                let size = decoder.position - start;
                bytecode = &bytecode[size..];
            }
        }

        decoder.into_visitor()
    }

    /// TODO FITZGEN
    pub fn into_visitor(self) -> V {
        self.visitor
    }
}

macro_rules! define_decoder {
    (
        $(
            $( #[$attr:meta] )*
                $snake_name:ident = $name:ident $( {
                $(
                    $( #[$field_attr:meta] )*
                    $field:ident : $field_ty:ty
                ),*
            } )? ;
        )*
    ) => {
        impl<V> Decoder<V>
        where
            V: OpVisitor,
        {
            unsafe fn decode_raw<T>(&mut self, mut bytecode: &[u8]) -> Result<V::Return, T::Error>
            where
                T: DecodeRawMethods
            {
                let start = self.position;

                let byte = bytecode.get(0).copied().ok_or_else(|| T::unexpected_eof(self.position))?;
                self.position += 1;
                bytecode = &bytecode[1..];

                let opcode = Opcode::new(byte).ok_or_else(|| T::invalid_opcode(self.position, byte))?;

                match opcode {
                    $(
                        Opcode::$name => {
                            $(
                                $(
                                    let (field_size, $field) = <$field_ty>::decode::<T>(
                                        self.position,
                                        bytecode,
                                    )?;
                                    self.position += field_size;
                                    #[allow(unused_assignments)]
                                    {
                                        bytecode = &bytecode[field_size..];
                                    }
                                )*
                            )?

                            let size = self.position - start;
                            self.visitor.before_visit(size);
                            let ret = self.visitor.$snake_name($( $( $field ),* )?);
                            self.visitor.after_visit(size);
                            Ok(ret)
                        }
                    )*
                }
            }
        }

        /// TODO FITZGEN
        pub trait OpVisitor {
            /// TODO FITZGEN
            type Return;

            /// TODO FITZGEN
            fn before_visit(&mut self, size: usize) {
                let _ = size;
            }

            /// TODO FITZGEN
            fn after_visit(&mut self, size: usize) {
                let _ = size;
            }

            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return;
            )*
        }
    };
}
for_each_op!(define_decoder);

macro_rules! define_extended_decoder {
    (
        $(
            $( #[$attr:meta] )*
                $snake_name:ident = $name:ident $( {
                $(
                    $( #[$field_attr:meta] )*
                    $field:ident : $field_ty:ty
                ),*
            } )? ;
        )*
    ) => {
        impl<V> Decoder<V>
        where
            V: ExtendedOpVisitor,
        {
            unsafe fn decode_extended_raw<T>(&mut self, mut bytecode: &[u8]) -> Result<V::Return, T::Error>
            where
                T: DecodeRawMethods
            {
                let start = self.position;

                let (size, code) = u16::decode::<T>(self.position, bytecode)?;
                self.position += size;
                #[allow(unused_assignments)]
                {
                    bytecode = &bytecode[size..];
                }

                let opcode = ExtendedOpcode::new(code).ok_or_else(|| {
                    T::invalid_extended_opcode(self.position, code)
                })?;

                match opcode {
                    $(
                        ExtendedOpcode::$name => {
                            $(
                                $(
                                    let (field_size, $field) = <$field_ty>::decode::<T>(
                                        self.position,
                                        bytecode,
                                    )?;
                                    self.position += field_size;
                                    #[allow(unused_assignments)]
                                    {
                                        bytecode = &bytecode[size..];
                                    }
                                )*
                            )?

                            let size = self.position - start;
                            self.visitor.before_visit(size);
                            let ret = self.visitor.$snake_name($( $( $field ),* )?);
                            self.visitor.after_visit(size);
                            Ok(ret)
                        }
                    )*
                }
            }
        }

        /// TODO FITZGEN
        pub trait ExtendedOpVisitor: OpVisitor {
            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return;
            )*
        }
    };
}
for_each_extended_op!(define_extended_decoder);
