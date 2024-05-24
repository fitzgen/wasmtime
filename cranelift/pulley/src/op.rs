//! Pulley bytecode operations with their operands.

use crate::imms::*;
use crate::regs::*;
#[allow(unused_imports)] // Some `cfg`s don't use this.
use alloc::vec::Vec;

macro_rules! define_op {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        /// TODO FITZGEN
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        pub enum Op {
            $(
                $( #[$attr] )*
                $name($name),
            )*
            /// TODO FITZGEN
            ExtendedOp(ExtendedOp),
        }

        $(
            $( #[$attr] )*
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
            pub struct $name { $(
                $(
                    // TODO: add doc comments to all fields and update all
                    // the macros to match them.
                    #[allow(missing_docs)]
                    pub $field : $field_ty,
                )*
            )? }
        )*
    };
}
for_each_op!(define_op);

impl From<ExtendedOp> for Op {
    #[inline]
    fn from(op: ExtendedOp) -> Self {
        Op::ExtendedOp(op)
    }
}

macro_rules! define_extended_op {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        /// TODO FITZGEN
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        pub enum ExtendedOp {
            $(
                $( #[$attr] )*
                $name($name),
            )*
        }

        $(
            $( #[$attr] )*
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
            pub struct $name { $(
                $(
                    // TODO: add doc comments to all fields and update all
                    // the macros to match them.
                    #[allow(missing_docs)]
                    pub $field : $field_ty,
                )*
            )? }
        )*
    };
}
for_each_extended_op!(define_extended_op);

macro_rules! define_op_encode {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        impl Op {
            /// TODO FITZGEN
            #[cfg(feature = "encode")]
            pub fn encode<E>(&self, into: &mut E)
            where
                E: Extend<u8>,
            {
                match self {
                    $(
                        Self::$name(op) => op.encode(into),
                    )*
                    Self::ExtendedOp(op) => op.encode(into),
                }
            }
        }

        $(
            impl $name {
                /// TODO FITZGEN
                #[cfg(feature = "encode")]
                pub fn encode<E>(&self, into: &mut E)
                where
                    E: Extend<u8>,
                {
                    crate::encode::$snake_name(into $( $( , self.$field )* )?);
                }
            }
        )*
    };
}
for_each_op!(define_op_encode);

macro_rules! define_extended_op_encode {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        impl ExtendedOp {
            /// TODO FITZGEN
            #[cfg(feature = "encode")]
            pub fn encode<E>(&self, into: &mut E)
            where
                E: Extend<u8>,
            {
                match self {
                    $(
                        Self::$name(op) => op.encode(into),
                    )*
                }
            }
        }

        $(
            impl $name {
                /// TODO FITZGEN
                #[cfg(feature = "encode")]
                pub fn encode<E>(&self, into: &mut E)
                where
                    E: Extend<u8>,
                {
                    crate::encode::$snake_name(into $( $( , self.$field )* )?);
                }
            }
        )*
    };
}
for_each_extended_op!(define_extended_op_encode);

/// TODO FITZGEN
#[cfg(feature = "decode")]
#[derive(Default)]
pub struct MaterializeOpsVisitor<B> {
    bytecode: B,
}

#[cfg(feature = "decode")]
impl<B> MaterializeOpsVisitor<B> {
    /// TODO FITZGEN
    pub fn new(bytecode: B) -> Self {
        Self { bytecode }
    }
}

macro_rules! define_materialize_op_visitor {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        #[cfg(feature = "decode")]
        impl<B: crate::decode::Bytecode> crate::decode::OpVisitor for MaterializeOpsVisitor<B> {
            type Bytecode = B;

            fn bytecode(&mut self) -> &mut Self::Bytecode {
                &mut self.bytecode
            }

            type Return = crate::op::Op;

            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
                    crate::op::Op::$name(crate::op::$name { $( $(
                        $field,
                    )* )? })
                }
            )*
        }
    };
}
for_each_op!(define_materialize_op_visitor);

macro_rules! define_materialize_extended_op_visitor {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        #[cfg(feature = "decode")]
        impl<B: crate::decode::Bytecode> crate::decode::ExtendedOpVisitor for MaterializeOpsVisitor<B> {
            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
                    crate::op::ExtendedOp::$name(crate::op::$name { $( $(
                        $field,
                    )* )? }).into()
                }
            )*
        }
    };
}
for_each_extended_op!(define_materialize_extended_op_visitor);
