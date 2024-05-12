//! Unwind information for pbc64.

use crate::isa::riscv64::inst::regs;
use crate::isa::unwind::systemv::RegisterMappingError;
use crate::machinst::Reg;
use gimli::{
    write::{CallFrameInstruction, CommonInformationEntry},
    Encoding, Format, Register,
};
use regalloc2::RegClass;

/// Creates a new riscv64 common information entry (CIE).
pub fn create_cie() -> CommonInformationEntry {
    todo!()
}

/// Map Cranelift registers to their corresponding Gimli registers.
pub fn map_reg(reg: Reg) -> Result<Register, RegisterMappingError> {
    let reg_offset = match reg.class() {
        RegClass::Int => 0,
        RegClass::Float => 64,
        RegClass::Vector => 128,
    };

    let reg = u16::from(reg.to_real_reg().unwrap().hw_enc());
    Ok(Register(reg_offset + reg))
}

pub(crate) struct RegisterMapper;

impl crate::isa::unwind::systemv::RegisterMapper<Reg> for RegisterMapper {
    fn map(&self, reg: Reg) -> Result<u16, RegisterMappingError> {
        Ok(map_reg(reg)?.0)
    }
    fn fp(&self) -> Option<u16> {
        Some(regs::fp_reg().to_real_reg().unwrap().hw_enc().into())
    }
    fn lr(&self) -> Option<u16> {
        Some(regs::link_reg().to_real_reg().unwrap().hw_enc().into())
    }
    fn lr_offset(&self) -> Option<u32> {
        todo!()
    }
}
