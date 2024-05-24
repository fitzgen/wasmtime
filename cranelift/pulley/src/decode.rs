//! Decoding support for pulley bytecode.

use crate::imms::*;
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
    type Bytecode<'a>: Copy;
    fn advance(bytecode: &mut Self::Bytecode<'_>, position: &mut usize, bytes: usize);
    fn get1(bytecode: &mut Self::Bytecode<'_>, position: &mut usize) -> Result<u8, Self::Error>;
    fn get2(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 2], Self::Error>;
    fn get4(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 4], Self::Error>;
    fn get8(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 8], Self::Error>;

    type Error;
    fn unexpected_eof(position: usize) -> Self::Error;
    fn invalid_opcode(position: usize, code: u8) -> Self::Error;
    fn invalid_extended_opcode(position: usize, code: u16) -> Self::Error;
    fn invalid_reg(position: usize, reg: u8) -> Self::Error;
}

struct SafeDecodeRawMethods;
impl DecodeRawMethods for SafeDecodeRawMethods {
    type Bytecode<'a> = &'a [u8];
    fn advance(bytecode: &mut Self::Bytecode<'_>, position: &mut usize, bytes: usize) {
        *bytecode = &bytecode.get(bytes..).unwrap();
        *position += bytes;
    }
    fn get1(bytecode: &mut Self::Bytecode<'_>, position: &mut usize) -> Result<u8, Self::Error> {
        let byte = bytecode
            .get(0)
            .copied()
            .ok_or_else(|| Self::unexpected_eof(*position))?;
        Self::advance(bytecode, position, 1);
        Ok(byte)
    }
    fn get2(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 2], Self::Error> {
        let a = Self::get1(bytecode, position)?;
        let b = Self::get1(bytecode, position)?;
        Ok([a, b])
    }
    fn get4(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 4], Self::Error> {
        let [a, b] = Self::get2(bytecode, position)?;
        let [c, d] = Self::get2(bytecode, position)?;
        Ok([a, b, c, d])
    }
    fn get8(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 8], Self::Error> {
        let [a, b, c, d] = Self::get4(bytecode, position)?;
        let [e, f, g, h] = Self::get4(bytecode, position)?;
        Ok([a, b, c, d, e, f, g, h])
    }

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
    type Bytecode<'a> = *mut u8;
    fn advance(bytecode: &mut Self::Bytecode<'_>, position: &mut usize, bytes: usize) {
        unsafe {
            *bytecode = bytecode.add(bytes);
            *position += bytes;
        }
    }
    fn get1(bytecode: &mut Self::Bytecode<'_>, position: &mut usize) -> Result<u8, Self::Error> {
        debug_assert!(!bytecode.is_null());
        let ret = unsafe { **bytecode };
        Self::advance(bytecode, position, 1);
        Ok(ret)
    }
    fn get2(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 2], Self::Error> {
        debug_assert!(!bytecode.is_null());
        let ret = unsafe { *bytecode.cast() };
        Self::advance(bytecode, position, 2);
        Ok(ret)
    }
    fn get4(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 4], Self::Error> {
        debug_assert!(!bytecode.is_null());
        let ret = unsafe { *bytecode.cast() };
        Self::advance(bytecode, position, 4);
        Ok(ret)
    }
    fn get8(
        bytecode: &mut Self::Bytecode<'_>,
        position: &mut usize,
    ) -> Result<[u8; 8], Self::Error> {
        debug_assert!(!bytecode.is_null());
        let ret = unsafe { *bytecode.cast() };
        Self::advance(bytecode, position, 8);
        Ok(ret)
    }

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
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods;
}

impl Decode for u8 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        T::get1(bytecode, position)
    }
}

impl Decode for u16 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u16_bytes = T::get2(bytecode, position)?;
        Ok(u16::from_le_bytes(u16_bytes))
    }
}

impl Decode for u32 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u32_bytes = T::get4(bytecode, position)?;
        Ok(u32::from_le_bytes(u32_bytes))
    }
}

impl Decode for u64 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u64_bytes = T::get8(bytecode, position)?;
        Ok(u64::from_le_bytes(u64_bytes))
    }
}

impl Decode for i8 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = T::get1(bytecode, position)?;
        Ok(byte as i8)
    }
}

impl Decode for i16 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u16_bytes = T::get2(bytecode, position)?;
        Ok(i16::from_le_bytes(u16_bytes))
    }
}

impl Decode for i32 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u32_bytes = T::get4(bytecode, position)?;
        Ok(i32::from_le_bytes(u32_bytes))
    }
}

impl Decode for i64 {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let u64_bytes = T::get8(bytecode, position)?;
        Ok(i64::from_le_bytes(u64_bytes))
    }
}

impl Decode for XReg {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = u8::decode::<T>(position, bytecode)?;
        XReg::new(byte).ok_or_else(|| T::invalid_reg(*position - 1, byte))
    }
}

impl Decode for FReg {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = u8::decode::<T>(position, bytecode)?;
        FReg::new(byte).ok_or_else(|| T::invalid_reg(*position - 1, byte))
    }
}

impl Decode for VReg {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        let byte = u8::decode::<T>(position, bytecode)?;
        VReg::new(byte).ok_or_else(|| T::invalid_reg(*position - 1, byte))
    }
}

impl Decode for PcRelOffset {
    fn decode<T>(position: &mut usize, bytecode: &mut T::Bytecode<'_>) -> Result<Self, T::Error>
    where
        T: DecodeRawMethods,
    {
        i32::decode::<T>(position, bytecode).map(|x| Self::from(x))
    }
}

/// TODO FITZGEN
pub struct Decoder {
    position: usize,
}

impl Decoder {
    /// TODO FITZGEN
    pub fn new() -> Self {
        Self { position: 0 }
    }

    /// TODO FITZGEN
    pub fn position(&self) -> usize {
        self.position
    }

    /// TODO FITZGEN
    pub fn set_position(&mut self, position: usize) {
        self.position = position;
    }

    /// TODO FITZGEN
    pub fn decode_one<V>(&mut self, bytecode: &mut &[u8], visitor: &mut V) -> Result<V::Return>
    where
        V: OpVisitor + ExtendedOpVisitor,
    {
        unsafe { self.decode_raw::<SafeDecodeRawMethods, V>(bytecode, visitor) }
    }

    /// TODO FITZGEN
    pub fn decode_one_extended<V>(
        &mut self,
        bytecode: &mut &[u8],
        visitor: &mut V,
    ) -> Result<V::Return>
    where
        V: OpVisitor + ExtendedOpVisitor,
    {
        unsafe { self.decode_extended_raw::<SafeDecodeRawMethods, V>(bytecode, visitor) }
    }

    /// TODO FITZGEN
    pub fn decode_all<V>(mut bytecode: &[u8], visitor: &mut V) -> Result<()>
    where
        V: OpVisitor + ExtendedOpVisitor,
    {
        let mut decoder = Decoder::new();

        while !bytecode.is_empty() {
            let is_extended_op = match bytecode.get(0).copied().and_then(Opcode::new) {
                Some(Opcode::ExtendedOp) => true,
                _ => false,
            };

            decoder.decode_one(&mut bytecode, visitor)?;

            if is_extended_op {
                decoder.decode_one_extended(&mut bytecode, visitor)?;
            }
        }

        Ok(())
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_decode_one<V>(
        &mut self,
        bytecode: &mut *mut u8,
        visitor: &mut V,
    ) -> V::Return
    where
        V: OpVisitor + ExtendedOpVisitor,
    {
        match self.decode_raw::<UnsafeDecodeRawMethods, V>(bytecode, visitor) {
            Ok(x) => x,
            Err(uninhabited) => match uninhabited {},
        }
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_decode_one_extended<V>(
        &mut self,
        bytecode: &mut *mut u8,
        visitor: &mut V,
    ) -> V::Return
    where
        V: OpVisitor + ExtendedOpVisitor,
    {
        match self.decode_extended_raw::<UnsafeDecodeRawMethods, V>(bytecode, visitor) {
            Ok(x) => x,
            Err(uninhabited) => match uninhabited {},
        }
    }
}

/// An `OpVisitor` combinator to sequence one visitor and then another.
pub struct SequencedVisitor<'a, F, V1, V2> {
    join: F,
    v1: &'a mut V1,
    v2: &'a mut V2,
}

impl<'a, F, V1, V2> SequencedVisitor<'a, F, V1, V2> {
    /// Create a new sequenced visitor.
    pub fn new(join: F, v1: &'a mut V1, v2: &'a mut V2) -> Self {
        SequencedVisitor { join, v1, v2 }
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
        impl Decoder {
            unsafe fn decode_raw<T, V>(
                &mut self,
                bytecode: &mut T::Bytecode<'_>,
                visitor: &mut V,
            ) -> Result<V::Return, T::Error>
            where
                T: DecodeRawMethods,
                V: OpVisitor,
            {
                let start = self.position;

                let byte = T::get1(bytecode, &mut self.position)?;
                let opcode = Opcode::new(byte).ok_or_else(|| {
                    T::invalid_opcode(self.position - 1, byte)
                })?;

                match opcode {
                    $(
                        Opcode::$name => {
                            $(
                                $(
                                    let $field = <$field_ty>::decode::<T>(
                                        &mut self.position,
                                        bytecode,
                                    )?;
                                )*
                            )?

                            let size = self.position - start;
                            visitor.before_visit(size);
                            let ret = visitor.$snake_name($( $( $field ),* )?);
                            visitor.after_visit(size);
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

        impl<F, T, V1, V2> OpVisitor for SequencedVisitor<'_, F, V1, V2>
        where
            F: FnMut(V1::Return, V2::Return) -> T,
            V1: OpVisitor,
            V2: OpVisitor,
        {
            type Return = T;

            fn before_visit(&mut self, size: usize) {
                self.v1.before_visit(size);
                self.v2.before_visit(size);
            }

            fn after_visit(&mut self, size: usize) {
                self.v1.before_visit(size);
                self.v2.before_visit(size);
            }

            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
                    let a = self.v1.$snake_name( $( $( $field , )* )? );
                    let b = self.v2.$snake_name( $( $( $field , )* )? );
                    (self.join)(a, b)
                }
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
        impl Decoder {
            unsafe fn decode_extended_raw<T, V>(
                &mut self,
                bytecode: &mut T::Bytecode<'_>,
                visitor: &mut V,
            ) -> Result<V::Return, T::Error>
            where
                T: DecodeRawMethods,
                V: ExtendedOpVisitor,
            {
                let start = self.position;

                let code = u16::decode::<T>(&mut self.position, bytecode)?;
                let opcode = ExtendedOpcode::new(code).ok_or_else(|| {
                    T::invalid_extended_opcode(self.position, code)
                })?;

                match opcode {
                    $(
                        ExtendedOpcode::$name => {
                            $(
                                $(
                                    let $field = <$field_ty>::decode::<T>(
                                        &mut self.position,
                                        bytecode,
                                    )?;
                                )*
                            )?

                            let size = self.position - start;
                            visitor.before_visit(size);
                            let ret = visitor.$snake_name($( $( $field ),* )?);
                            visitor.after_visit(size);
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

        impl<F, T, V1, V2> ExtendedOpVisitor for SequencedVisitor<'_, F, V1, V2>
        where
            F: FnMut(V1::Return, V2::Return) -> T,
            V1: ExtendedOpVisitor,
            V2: ExtendedOpVisitor,
        {
            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
                    let a = self.v1.$snake_name( $( $( $field , )* )? );
                    let b = self.v2.$snake_name( $( $( $field , )* )? );
                    (self.join)(a, b)
                }
            )*
        }
    };
}
for_each_extended_op!(define_extended_decoder);
