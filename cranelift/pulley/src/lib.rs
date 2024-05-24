//! The pulley bytecode for fast interpreters.

#![deny(missing_docs)]
#![no_std]

#[cfg(feature = "std")]
#[macro_use]
extern crate std;

#[allow(unused_extern_crates)] // Some cfg's don't use this.
extern crate alloc;

// TODO: make `struct BinaryOpRegs<T> { dst: T, src1: T, src2: T }` or something
// and pack it into 2 bytes (5 bits for each operand; 2**5 = 32 possible
// registers, and then only special isntructions to access special x registers)

/// Calls the given macro with each opcode.
macro_rules! for_each_op {
    ( $macro:ident ) => {
        $macro! {
            /// TODO FITZGEN
            ret = Ret;
            /// TODO FITZGEN
            call = Call { offset: PcRelOffset };
            /// TODO FITZGEN
            jump = Jump { offset: PcRelOffset };
            /// TODO FITZGEN
            br_if = BrIf { cond: XReg, offset: PcRelOffset };
            /// TODO FITZGEN
            br_if_not = BrIfNot { cond: XReg, offset: PcRelOffset };

            /// TODO FITZGEN
            xmov = Xmov { dst: XReg, src: XReg };
            /// TODO FITZGEN
            fmov = Fmov { dst: FReg, src: FReg };
            /// TODO FITZGEN
            vmov = Vmov { dst: VReg, src: VReg };

            /// TODO FITZGEN
            xconst8 = Xconst8 { dst: XReg, imm: u8 };
            /// TODO FITZGEN
            xconst16 = Xconst16 { dst: XReg, imm: u16 };
            /// TODO FITZGEN
            xconst32 = Xconst32 { dst: XReg, imm: u32 };
            /// TODO FITZGEN
            xconst64 = Xconst64 { dst: XReg, imm: u64 };

            /// TODO FITZGEN
            xadd32 = Xadd32 { dst: XReg, src1: XReg, src2: XReg };
            /// TODO FITZGEN
            xadd64 = Xadd64 { dst: XReg, src1: XReg, src2: XReg };

            /// Equal.
            xeq64 = Xeq64 { dst: XReg, src1: XReg, src2: XReg };
            /// Not equal.
            xneq64 = Xneq64 { dst: XReg, src1: XReg, src2: XReg };
            /// Signed less-than.
            xslt64 = Xslt64 { dst: XReg, src1: XReg, src2: XReg };
            /// Signed less-than-equal.
            xslteq64 = Xslteq64 { dst: XReg, src1: XReg, src2: XReg };
            /// Unsigned less-than.
            xult64 = Xult64 { dst: XReg, src1: XReg, src2: XReg };
            /// Unsigned less-than-equal.
            xulteq64 = Xulteq64 { dst: XReg, src1: XReg, src2: XReg };
            /// Equal.
            xeq32 = Xeq32 { dst: XReg, src1: XReg, src2: XReg };
            /// Not equal.
            xneq32 = Xneq32 { dst: XReg, src1: XReg, src2: XReg };
            /// Signed less-than.
            xslt32 = Xslt32 { dst: XReg, src1: XReg, src2: XReg };
            /// Signed less-than-equal.
            xslteq32 = Xslteq32 { dst: XReg, src1: XReg, src2: XReg };
            /// Unsigned less-than.
            xult32 = Xult32 { dst: XReg, src1: XReg, src2: XReg };
            /// Unsigned less-than-equal.
            xulteq32 = Xulteq32 { dst: XReg, src1: XReg, src2: XReg };

            /// TODO FITZGEN
            load32_u = LoadU32 { dst: XReg, ptr: XReg };
            /// TODO FITZGEN
            load32_s = LoadS32 { dst: XReg, ptr: XReg };
            /// TODO FITZGEN
            load64 = Load64 { dst: XReg, ptr: XReg };

            /// TODO FITZGEN
            load32_u_offset8 = Load32UOffset8 { dst: XReg, ptr: XReg, offset: i8 };
            /// TODO FITZGEN
            load32_s_offset8 = Load32SOffset8 { dst: XReg, ptr: XReg, offset: i8 };
            /// TODO FITZGEN
            load64_offset8 = Load64Offset8 { dst: XReg, ptr: XReg, offset: i8 };

            /// TODO FITZGEN
            load32_u_offset16 = Load32UOffset16 { dst: XReg, ptr: XReg, offset: i16 };
            /// TODO FITZGEN
            load32_s_offset16 = Load32SOffset16 { dst: XReg, ptr: XReg, offset: i16 };
            /// TODO FITZGEN
            load64_offset16 = Load64Offset16 { dst: XReg, ptr: XReg, offset: i16 };

            /// TODO FITZGEN
            load32_u_offset32 = Load32UOffset32 { dst: XReg, ptr: XReg, offset: i32 };
            /// TODO FITZGEN
            load32_s_offset32 = Load32SOffset32 { dst: XReg, ptr: XReg, offset: i32 };
            /// TODO FITZGEN
            load64_offset32 = Load64Offset32 { dst: XReg, ptr: XReg, offset: i32 };

            /// TODO FITZGEN
            load32_u_offset64 = Load32UOffset64 { dst: XReg, ptr: XReg, offset: i64 };
            /// TODO FITZGEN
            load32_s_offset64 = Load32SOffset64 { dst: XReg, ptr: XReg, offset: i64 };
            /// TODO FITZGEN
            load64_offset64 = Load64Offset64 { dst: XReg, ptr: XReg, offset: i64 };

            /// TODO FITZGEN
            store32 = StoreS32 { ptr: XReg, src: XReg };
            /// TODO FITZGEN
            store64 = Store64 { ptr: XReg, src: XReg };

            /// TODO FITZGEN
            store32_offset8 = Store32SOffset8 { ptr: XReg, offset: i8, src: XReg };
            /// TODO FITZGEN
            store64_offset8 = Store64Offset8 { ptr: XReg, offset: i8, src: XReg };

            /// TODO FITZGEN
            store32_offset16 = Store32Offset16 { ptr: XReg, offset: i16, src: XReg };
            /// TODO FITZGEN
            store64_offset16 = Store64Offset16 { ptr: XReg, offset: i16, src: XReg };

            /// TODO FITZGEN
            store32_offset32 = Store32Offset32 { ptr: XReg, offset: i32, src: XReg };
            /// TODO FITZGEN
            store64_offset32 = Store64Offset32 { ptr: XReg, offset: i32, src: XReg };

            /// TODO FITZGEN
            store32_offset64 = Store32Offset64 { ptr: XReg, offset: i64, src: XReg };
            /// TODO FITZGEN
            store64_offset64 = Store64Offset64 { ptr: XReg, offset: i64, src: XReg };

            /// TODO FITZGEN
            bitcast_int_from_float_32 = BitcastIntFromFloat32 { dst: XReg, src: FReg };
            /// TODO FITZGEN
            bitcast_int_from_float_64 = BitcastIntFromFloat64 { dst: XReg, src: FReg };
            /// TODO FITZGEN
            bitcast_float_from_int_32 = BitcastFloatFromInt32 { dst: FReg, src: XReg };
            /// TODO FITZGEN
            bitcast_float_from_int_64 = BitcastFloatFromInt64 { dst: FReg, src: XReg };
        }
    };
}

/// Calls the given macro with each extended opcode.
macro_rules! for_each_extended_op {
    ( $macro:ident ) => {
        $macro! {
            /// TODO FITZGEN
            trap = Trap;

            /// TODO FITZGEN
            nop = Nop;

            /// TODO FITZGEN
            get_sp = GetSp { dst: XReg };
        }
    };
}

#[cfg(feature = "decode")]
pub mod decode;
#[cfg(feature = "disas")]
pub mod disas;
#[cfg(feature = "encode")]
pub mod encode;
#[cfg(feature = "interp")]
pub mod interp;

pub mod regs;
pub use regs::*;

pub mod imms;
pub use imms::*;

pub mod op;
pub use op::*;

pub mod opcode;
pub use opcode::*;

#[allow(dead_code)] // Unused in some `cfg`s.
pub(crate) unsafe fn unreachable_unchecked<T>() -> T {
    #[cfg(debug_assertions)]
    unreachable!();

    #[cfg_attr(debug_assertions, allow(unreachable_code))]
    unsafe {
        core::hint::unreachable_unchecked()
    }
}
