//! TODO FITZGEN

use crate::{prelude::*, CompileTimeBuiltinValue};
use cranelift_entity::PrimaryMap;

/// TODO FITZGEN
#[derive(
    Clone, Default, Debug, Eq, PartialEq, Hash, serde_derive::Serialize, serde_derive::Deserialize,
)]
pub struct CompileTimeBuiltinData {
    /// TODO FITZGEN
    pub body: PrimaryMap<CompileTimeBuiltinValue, CompileTimeBuiltinInst>,
    /// TODO FITZGEN
    pub results: Vec<CompileTimeBuiltinValue>,
    // TODO FITZGEN: might need the function signature here
}

/// TODO FITZGEN
#[macro_export]
macro_rules! for_each_compile_time_builtin_inst {
    ( $mac:ident ) => {
        $mac! {
            /// Get the `i`th argument to this function, which must be a 32-bit integer.
            i32_arg = I32Arg { i: u32 } -> CompileTimeBuiltinValue,

            /// Get the `i`th argument to this function, which must be a 64-bit integer.
            i64_arg = I64Arg { i: u32 } -> CompileTimeBuiltinValue,

            /// Zero-extend the given 32-bit integer into a 64-bit integer.
            i32_zero_extend = I32ZeroExtend {
                value: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// Sign-extend the given 32-bit integer into a 64-bit integer.
            i32_sign_extend = I32SignExtend {
                value: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// 32-bit wrapping addition: `a + b`
            i32_add = I32Add {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// 64-bit wrapping addition: `a + b`
            i64_add = I64Add {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// 32-bit wrapping multiplication: `a * b`.
            i32_mul = I32Mul {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// 64-bit wrapping multiplication: `a * b`.
            i64_mul = I64Mul {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// Unsigned less-than comparison for 32-bit integers: `lhs < rhs`.
            i32_unsigned_less_than = I32UnsignedLessThan {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// Unsigned less-than comparison for 64-bit integers: `lhs < rhs`.
            i64_unsigned_less_than = I64UnsignedLessThan {
                lhs: CompileTimeBuiltinValue,
                rhs: CompileTimeBuiltinValue,
            } -> CompileTimeBuiltinValue,

            /// Trap if `condition == 0`.
            trap_if_zero = TrapIfZero {
                condition: CompileTimeBuiltinValue,
                code: crate::Trap,
            },

            /// Trap if `condition != 0`.
            trap_if_non_zero = TrapIfNonZero {
                condition: CompileTimeBuiltinValue,
                code: crate::Trap,
            },

            /// Load an 8-bit integer and sign-extend it to 32-bits.
            i32_signed_load8 = I32SignedLoad8 { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Load an 8-bit integer and zero-extend it to 32-bits.
            i32_unsigned_load8 = I32UnsignedLoad8 { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Load a 16-bit little-endian integer and sign-extend it to 32-bits.
            i32_signed_load16_le = I32SignedLoad16Le { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Load a 16-bit little-endian  integer and zero-extend it to 32-bits.
            i32_unsigned_load16_le = I32UnsignedLoad16Le { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Load a 32-bit, little-endian integer.
            i32_load_le = I32LoadLe { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Load a 64-bit, little-endian integer.
            i64_load_le = I64LoadLe { addr: CompileTimeBuiltinValue } -> CompileTimeBuiltinValue,

            /// Truncate a 32-bit integer to 8 bits and store it to memory.
            i32_store8 = I32Store8 {
                addr: CompileTimeBuiltinValue,
                value: CompileTimeBuiltinValue,
            },

            /// Truncate a 32-bit integer to 16 bits and store it to memory.
            i32_store16_le = I32Store16Le {
                addr: CompileTimeBuiltinValue,
                value: CompileTimeBuiltinValue,
            },

            /// Store a 32-bit integer to memory.
            i32_store_le = I32StoreLe {
                addr: CompileTimeBuiltinValue,
                value: CompileTimeBuiltinValue,
            },

            /// Store a 64-bit integer to memory.
            i64_store_le = I64StoreLe {
                addr: CompileTimeBuiltinValue,
                value: CompileTimeBuiltinValue,
            },
        }
    };
}

macro_rules! define_inst {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* $(,)? } )? $( -> $ret_ty:ty )?
        ),* $(,)?
    ) => {
        /// TODO FITZGEN
        #[allow(missing_docs)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, serde_derive::Serialize, serde_derive::Deserialize)]
        pub enum CompileTimeBuiltinInst {
            $(
                $( #[$attr] )*
                $name $( {
                    $(
                        #[expect(missing_docs, reason = "macro-generated code")]
                        $field : $field_ty
                    ),*
                } )? ,
            )*
        }
    }
}

for_each_compile_time_builtin_inst!(define_inst);
