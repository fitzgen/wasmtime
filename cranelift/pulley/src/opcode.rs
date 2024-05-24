//! Pulley opcodes (without operands).

macro_rules! define_opcode {
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
        /// TODO FITZGEN
        #[repr(u8)]
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Opcode {
            $(
                $( #[$attr] )*
                $name,
            )*
            /// TODO FITZGEN
            ExtendedOp,
        }

        impl Opcode {
            /// TODO FITZGEN
            pub const MAX: u8 = define_opcode!( @max $( $name )* ) + 1;
        }
    };

    ( @max $x:ident ) => { 0 };
    ( @max $x:ident $( $xs:ident )* ) => { 1 + define_opcode!(@max $( $xs )* ) };
}
for_each_op!(define_opcode);

impl Opcode {
    /// TODO FITZGEN
    pub fn new(byte: u8) -> Option<Self> {
        if byte <= Self::MAX {
            Some(unsafe { Self::unchecked_new(byte) })
        } else {
            None
        }
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_new(byte: u8) -> Self {
        debug_assert!(byte <= Self::MAX);
        core::mem::transmute(byte)
    }
}

macro_rules! define_extended_opcode {
    (
        $(
            $( #[$attr:meta] )*
            $snake_name:ident = $name:ident $( { $( $field:ident : $field_ty:ty ),* } )? ;
        )*
    ) => {
        /// TODO FITZGEN
        #[repr(u16)]
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum ExtendedOpcode {
            $(
                $( #[$attr] )*
                    $name,
            )*
        }

        impl ExtendedOpcode {
            /// TODO FITZGEN
            pub const MAX: u16 = define_opcode!( @max $( $name )* );
        }
    };

    ( @max $x:ident ) => { 0 };
    ( @max $x:ident $( $xs:ident )* ) => { 1 + define_opcode!(@max $( $xs )* ) };
}
for_each_extended_op!(define_extended_opcode);

impl ExtendedOpcode {
    /// TODO FITZGEN
    pub fn new(byte: u16) -> Option<Self> {
        if byte <= Self::MAX {
            Some(unsafe { Self::unchecked_new(byte) })
        } else {
            None
        }
    }

    /// TODO FITZGEN
    pub unsafe fn unchecked_new(byte: u16) -> Self {
        debug_assert!(byte <= Self::MAX);
        core::mem::transmute(byte)
    }
}
