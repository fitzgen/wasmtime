//! Disassembly support for pulley bytecode.

use crate::decode::*;
use crate::imms::*;
use crate::regs::*;
use alloc::string::String;
use core::fmt::Write;

/// TODO FITZGEN
pub struct Disassembler<'a> {
    raw_bytecode: &'a [u8],
    bytecode: SafeBytecode<'a>,
    disas: String,
    start: usize,
    temp: String,
}

impl<'a> Disassembler<'a> {
    /// TODO FITZGEN
    pub fn new(bytecode: &'a [u8]) -> Self {
        Self {
            raw_bytecode: bytecode,
            bytecode: SafeBytecode::new(bytecode),
            disas: String::new(),
            start: 0,
            temp: String::new(),
        }
    }

    /// TODO FITZGEN
    pub fn disas(&self) -> &str {
        &self.disas
    }
}

trait Disas {
    fn disas(&self, position: usize, disas: &mut String);
}

impl Disas for XReg {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for FReg {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for VReg {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for i8 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for i16 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for i32 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for i64 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for u8 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for u16 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for u32 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for u64 {
    fn disas(&self, _position: usize, disas: &mut String) {
        write!(disas, "{self}").unwrap();
    }
}

impl Disas for PcRelOffset {
    fn disas(&self, position: usize, disas: &mut String) {
        let offset = isize::try_from(i32::from(*self)).unwrap();
        let target = position.wrapping_add(offset as usize);
        write!(disas, "{offset:#x}    // target = {target:#x}").unwrap()
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
        impl<'a> OpVisitor for Disassembler<'a> {
            type Bytecode = SafeBytecode<'a>;

            fn bytecode(&mut self) -> &mut Self::Bytecode {
                &mut self.bytecode
            }

            type Return = ();

            fn before_visit(&mut self) {
                self.start = self.bytecode.position();
            }

            fn after_visit(&mut self) {
                let size = self.bytecode.position() - self.start;

                write!(&mut self.disas, "{:8x}: ", self.start).unwrap();
                let mut need_space = false;
                for byte in &self.raw_bytecode[self.start..][..size] {
                    write!(&mut self.disas, "{}{byte:02x}", if need_space { " " } else { "" }).unwrap();
                    need_space = true;
                }
                for _ in 0..11_usize.saturating_sub(size) {
                    write!(&mut self.disas, "   ").unwrap();
                }

                self.disas.push_str(&self.temp);
                self.temp.clear();

                self.disas.push('\n');
            }

            $(
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) {
                    let mnemonic = stringify!($snake_name);
                    write!(&mut self.temp, "{mnemonic}").unwrap();
                    $(
                        let mut need_comma = false;
                        $(
                            let val = $field;
                            if need_comma {
                                write!(&mut self.temp, ",").unwrap();
                            }
                            write!(&mut self.temp, " ").unwrap();
                            val.disas(self.start, &mut self.temp);
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
        impl<'a> ExtendedOpVisitor for Disassembler<'_> {
            $(
                fn $snake_name(&mut self $( $( , $field : $field_ty )* )? ) {
                    let mnemonic = stringify!($snake_name);
                    write!(&mut self.temp, "{mnemonic}").unwrap();
                    $(
                        let mut need_comma = false;
                        $(
                            let val = $field;
                            if need_comma {
                                write!(&mut self.temp, ",").unwrap();
                            }
                            write!(&mut self.temp, " ").unwrap();
                            val.disas(self.start, &mut self.temp);
                            #[allow(unused_assignments)]
                            { need_comma = true; }
                        )*
                    )?
                }
            )*
        }
    };
}
for_each_extended_op!(impl_extended_disas);
