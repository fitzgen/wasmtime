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
        }

        $(
            define_op! {
                @op_type
                $( #[$attr] )*
                $snake_name = $name $( { $( $field : $field_ty ),* } )? ;
            }
        )*
    };

    // Intercept the definition of `ExtendedOp`; its type already gets defined
    // by `define_extended_op!`.
    (
        @op_type
        $( #[$attr:meta] )*
        extended_op = ExtendedOp;
    ) => {};
    (
        @op_type
        $( #[$attr:meta] )*
        $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
    ) => {
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
            define_op! {
                @op_type
                $( #[$attr] )*
                $snake_name = $name $( { $( $field : $field_ty ),* } )? ;
            }
        )*
    };

    (
        @op_type
        $( #[$attr:meta] )*
        $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
    ) => {
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
                }
            }
        }

        $(
            define_op_encode! {
                @op_ty_encode
                $snake_name = $name $( { $( $field : $field_ty ),* } )? ;
            }
        )*
    };
    // Intercept `extended_op` and let `define_extended_op_encode!` handle this
    // case.
    (
        @op_ty_encode
        $( #[$attr:meta] )*
        extended_op = ExtendedOp;
    ) => {};
    (
        @op_ty_encode
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
    ) => {
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
pub struct MaterializeOpsVisitor;

macro_rules! define_materialize_op_visitor {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        #[cfg(feature = "decode")]
        impl crate::decode::OpVisitor for MaterializeOpsVisitor {
            type Return = Option<crate::op::Op>;

            $(
                define_materialize_op_visitor! {
                    @func
                    $snake_name = $name $( { $( $field : $field_ty ),* } )? ;
                }
            )*
        }
    };
    // Intercept `extended_op` and let the `ExtendedOpVisitor` take care of
    // materializing the extended op.
    (
        @func
        $( #[$attr:meta] )*
        extended_op = ExtendedOp;
    ) => {
        $( #[$attr] )*
        fn extended_op(&mut self) -> Self::Return { None }
    };
    (
        @func
        $( #[$attr:meta] )*
        $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
    ) => {
        $( #[$attr] )*
        fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
            Some(crate::op::Op::$name(crate::op::$name { $( $(
                $field,
            )* )? }))
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
        impl crate::decode::ExtendedOpVisitor for MaterializeOpsVisitor {
            $(
                $( #[$attr] )*
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) -> Self::Return {
                    Some(crate::op::ExtendedOp::$name(crate::op::$name { $( $(
                        $field,
                    )* )? }).into())
                }
            )*
        }
    };
}
for_each_extended_op!(define_materialize_extended_op_visitor);
