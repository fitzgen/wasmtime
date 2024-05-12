//! Disassembly support for pulley bytecode.

use crate::decode::*;
use crate::regs::*;
use alloc::string::String;
use core::fmt::Write;

/// TODO FITZGEN
pub struct Disassembler<'a> {
    bytecode: &'a [u8],
    position: usize,
    disas: String,
}

impl<'a> Disassembler<'a> {
    /// TODO FITZGEN
    pub fn new(bytecode: &'a [u8]) -> Self {
        Self {
            bytecode,
            position: 0,
            disas: String::new(),
        }
    }
}

macro_rules! impl_disas {
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
        impl<'a> OpVisitor for &'a mut Disassembler<'_> {
            type Return = ();

            fn before_visit(&mut self, size: usize) {
                write!(&mut self.disas, "{:8x}    ", self.position).unwrap();

                let mut need_space = false;
                for byte in &self.bytecode[self.position..][..size] {
                    write!(&mut self.disas, "{}{byte:x}", if need_space { " " } else { "" }).unwrap();
                    need_space = true;
                }
                for _ in 0..6 - size {
                    write!(&mut self.disas, "   ").unwrap();
                }
            }

            fn after_visit(&mut self, size: usize) {
                self.disas.push('\n');
                self.position += size;
            }

            $(
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) {
                    let mnemonic = stringify!($snake_name);
                    write!(&mut self.disas, "{mnemonic}").unwrap();
                    $(
                        let mut need_comma = false;
                        $(
                            let field = stringify!($field);
                            let val = $field;
                            write!(
                                &mut self.disas,
                                "{} {field}={val}",
                                if need_comma { "," } else { "" },
                            ).unwrap();
                            #[allow(unused_assignments)]
                            { need_comma = true; }
                        )*
                    )?
                }
            )*
        }
    };
}
for_each_op!(impl_disas);

macro_rules! impl_extended_disas {
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
        impl<'a> ExtendedOpVisitor for &'a mut Disassembler<'_> {
            $(
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) {
                    let mnemonic = stringify!($snake_name);
                    write!(&mut self.disas, "\t{mnemonic}").unwrap();
                    $(
                        let mut need_comma = false;
                        $(
                            let field = stringify!($field);
                            let val = $field;
                            write!(
                                &mut self.disas,
                                "{} {field}={val}",
                                if need_comma { "," } else { "" },
                            ).unwrap();
                            need_comma = true;
                        )*
                    )?
                }
            )*
        }
    };
}
for_each_extended_op!(impl_extended_disas);
