//! This module defines pbc64-specific machine instruction types.

use crate::binemit::{Addend, CodeOffset, Reloc};
use crate::ir::types::{self, F32, F64, I128, I16, I32, I64, I8, I8X16, R32, R64};
use crate::ir::{self, ExternalName, MemFlags, Opcode, Type};
use crate::isa::pbc64::abi::Pbc64MachineDeps;
use crate::isa::{CallConv, FunctionAlignment};
use crate::{machinst::*, trace};
use crate::{settings, CodegenError, CodegenResult};
use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use regalloc2::{PRegSet, RegClass};
use smallvec::{smallvec, SmallVec};

pub mod regs;
pub use self::regs::*;
pub mod imms;
pub use self::imms::*;
pub mod args;
pub use self::args::*;
pub mod emit;
pub use self::emit::*;
pub mod unwind;

pub use crate::ir::condcodes::{FloatCC, IntCC};

pub(crate) type VecU8 = Vec<u8>;

//=============================================================================
// Instructions (top level): definition

pub use crate::isa::pbc64::lower::isle::generated_code::MInst as Inst;

/// Additional information for direct and indirect call instructions.
///
/// Left out of line to lower the size of the `Inst` enum.
#[derive(Clone, Debug)]
pub struct CallInfo {
    pub uses: CallArgList,
    pub defs: CallRetList,
    pub opcode: Opcode,
    pub clobbers: PRegSet,
    pub callee_pop_size: u32,
}

/// Similar to `CallInfo` but for return calls.
#[derive(Clone, Debug)]
pub struct ReturnCallInfo {
    pub uses: CallArgList,
    pub opcode: Opcode,
    pub new_stack_arg_size: u32,
}

/// A conditional branch target.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CondBrTarget {
    /// An unresolved reference to a Label, as passed into
    /// `lower_branch_group()`.
    Label(MachLabel),
    /// No jump; fall through to the next instruction.
    Fallthrough,
}

impl CondBrTarget {
    /// Return the target's label, if it is a label-based target.
    pub(crate) fn as_label(self) -> Option<MachLabel> {
        match self {
            CondBrTarget::Label(l) => Some(l),
            _ => None,
        }
    }

    pub(crate) fn is_fallthrouh(&self) -> bool {
        self == &CondBrTarget::Fallthrough
    }
}

impl core::fmt::Display for CondBrTarget {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            CondBrTarget::Label(l) => write!(f, "{}", l.to_string()),
            CondBrTarget::Fallthrough => write!(f, "0"),
        }
    }
}

/// rd and src must have the same length.
pub(crate) fn gen_moves(rd: &[Writable<Reg>], src: &[Reg]) -> SmallInstVec<Inst> {
    assert!(rd.len() == src.len());
    assert!(rd.len() > 0);
    let mut insts = SmallInstVec::new();
    for (dst, src) in rd.iter().zip(src.iter()) {
        let ty = Inst::canonical_type_for_rc(dst.to_reg().class());
        insts.push(Inst::gen_move(*dst, *src, ty));
    }
    insts
}

impl Inst {
    /// Generic constructor for a load (zero-extending where appropriate).
    pub fn gen_load(dst: Writable<Reg>, mem: Amode, ty: Type, flags: MemFlags) -> Inst {
        Inst::Load {
            dst,
            mem,
            ty,
            flags,
            ext: ExtKind::Zero,
        }
    }

    /// Generic constructor for a store.
    pub fn gen_store(mem: Amode, from_reg: Reg, ty: Type, flags: MemFlags) -> Inst {
        Inst::Store {
            mem,
            src: from_reg,
            ty,
            flags,
        }
    }
}

fn pbc64_get_operands(inst: &mut Inst, collector: &mut impl OperandVisitor) {
    match inst {
        Inst::Args { args } => {
            for ArgPair { vreg, preg } in args {
                collector.reg_fixed_def(vreg, *preg);
            }
        }
        Inst::Rets { rets } => {
            for RetPair { vreg, preg } in rets {
                collector.reg_fixed_use(vreg, *preg);
            }
        }
        Inst::Ret => {
            unreachable!("`ret` is only added after regalloc")
        }

        Inst::Unwind { .. } | Inst::Trap { .. } | Inst::Nop => {}

        Inst::GetSp { dst } => {
            collector.reg_def(dst);
        }

        Inst::LoadExtName {
            dst,
            name: _,
            offset: _,
        } => {
            collector.reg_def(dst);
        }

        Inst::Call { callee: _, info } => {
            let CallInfo { uses, defs, .. } = &mut **info;
            for CallArgPair { vreg, preg } in uses {
                collector.reg_fixed_use(vreg, *preg);
            }
            for CallRetPair { vreg, preg } in defs {
                collector.reg_fixed_def(vreg, *preg);
            }
            collector.reg_clobbers(info.clobbers);
        }
        Inst::IndirectCall { callee, info } => {
            collector.reg_use(callee);
            let CallInfo { uses, defs, .. } = &mut **info;
            for CallArgPair { vreg, preg } in uses {
                collector.reg_fixed_use(vreg, *preg);
            }
            for CallRetPair { vreg, preg } in defs {
                collector.reg_fixed_def(vreg, *preg);
            }
            collector.reg_clobbers(info.clobbers);
        }

        Inst::Jump { .. } => {}

        Inst::BrIf {
            c,
            taken: _,
            not_taken: _,
        } => {
            collector.reg_use(c);
        }

        Inst::Xmov { dst, src } => todo!(),
        Inst::Fmov { dst, src } => todo!(),
        Inst::Vmov { dst, src } => todo!(),

        Inst::Xconst8 { dst, imm: _ }
        | Inst::Xconst16 { dst, imm: _ }
        | Inst::Xconst32 { dst, imm: _ }
        | Inst::Xconst64 { dst, imm: _ } => {
            collector.reg_def(dst);
        }

        Inst::Xadd32 { dst, src1, src2 }
        | Inst::Xadd64 { dst, src1, src2 }
        | Inst::Xeq64 { dst, src1, src2 }
        | Inst::Xneq64 { dst, src1, src2 }
        | Inst::Xslt64 { dst, src1, src2 }
        | Inst::Xslteq64 { dst, src1, src2 }
        | Inst::Xult64 { dst, src1, src2 }
        | Inst::Xulteq64 { dst, src1, src2 }
        | Inst::Xeq32 { dst, src1, src2 }
        | Inst::Xneq32 { dst, src1, src2 }
        | Inst::Xslt32 { dst, src1, src2 }
        | Inst::Xslteq32 { dst, src1, src2 }
        | Inst::Xult32 { dst, src1, src2 }
        | Inst::Xulteq32 { dst, src1, src2 } => {
            collector.reg_use(src1);
            collector.reg_use(src2);
            collector.reg_def(dst);
        }

        Inst::Load {
            dst,
            mem,
            ty: _,
            flags: _,
            ext: _,
        } => {
            collector.reg_def(dst);
            mem.get_operands(collector);
        }

        Inst::Store {
            mem,
            src,
            ty: _,
            flags: _,
        } => {
            mem.get_operands(collector);
            collector.reg_use(src);
        }

        Inst::BitcastIntFromFloat32 { dst, src } => todo!(),
        Inst::BitcastIntFromFloat64 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt32 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt64 { dst, src } => todo!(),
    }
}

impl MachInst for Inst {
    type LabelUse = LabelUse;
    type ABIMachineSpec = Pbc64MachineDeps;

    const TRAP_OPCODE: &'static [u8] = &[0];

    fn gen_dummy_use(reg: Reg) -> Self {
        todo!()
    }

    fn canonical_type_for_rc(rc: RegClass) -> Type {
        match rc {
            regalloc2::RegClass::Int => I64,
            regalloc2::RegClass::Float => F64,
            regalloc2::RegClass::Vector => I8X16,
        }
    }

    fn is_safepoint(&self) -> bool {
        match self {
            Inst::Trap { .. } => true,
            _ => false,
        }
    }

    fn get_operands(&mut self, collector: &mut impl OperandVisitor) {
        pbc64_get_operands(self, collector);
    }

    fn is_move(&self) -> Option<(Writable<Reg>, Reg)> {
        match self {
            Inst::Xmov { dst, src } => Some((Writable::from_reg(*dst.to_reg()), **src)),
            _ => None,
        }
    }

    fn is_included_in_clobbers(&self) -> bool {
        self.is_args()
    }

    fn is_trap(&self) -> bool {
        match self {
            Inst::Trap { .. } => true,
            _ => false,
        }
    }

    fn is_args(&self) -> bool {
        match self {
            Inst::Args { .. } => false,
            _ => true,
        }
    }

    fn is_term(&self) -> MachTerminator {
        match self {
            Inst::Ret { .. } | Inst::Rets { .. } => MachTerminator::Ret,
            Inst::Jump { .. } => MachTerminator::Uncond,
            Inst::BrIf { .. } => MachTerminator::Cond,
            _ => MachTerminator::None,
        }
    }

    fn is_mem_access(&self) -> bool {
        todo!()
    }

    fn gen_move(to_reg: Writable<Reg>, from_reg: Reg, ty: Type) -> Inst {
        match ty {
            ir::types::I8 | ir::types::I16 | ir::types::I32 | ir::types::I64 => Inst::Xmov {
                dst: WritableXReg::try_from(to_reg).unwrap(),
                src: XReg::new(from_reg).unwrap(),
            },
            ir::types::F32 | ir::types::F64 => Inst::Fmov {
                dst: WritableFReg::try_from(to_reg).unwrap(),
                src: FReg::new(from_reg).unwrap(),
            },
            _ if ty.is_vector() => Inst::Vmov {
                dst: WritableVReg::try_from(to_reg).unwrap(),
                src: VReg::new(from_reg).unwrap(),
            },
            _ => panic!("don't know how to generate a move for type {ty}"),
        }
    }

    fn gen_nop(preferred_size: usize) -> Inst {
        todo!()
    }

    fn rc_for_type(ty: Type) -> CodegenResult<(&'static [RegClass], &'static [Type])> {
        match ty {
            I8 => Ok((&[RegClass::Int], &[I8])),
            I16 => Ok((&[RegClass::Int], &[I16])),
            I32 => Ok((&[RegClass::Int], &[I32])),
            I64 => Ok((&[RegClass::Int], &[I64])),
            R32 => panic!("32-bit reftype pointer should never be seen on pbc64"),
            R64 => Ok((&[RegClass::Int], &[R64])),
            F32 => Ok((&[RegClass::Float], &[F32])),
            F64 => Ok((&[RegClass::Float], &[F64])),
            I128 => Ok((&[RegClass::Int, RegClass::Int], &[I64, I64])),
            _ if ty.is_vector() => {
                debug_assert!(ty.bits() <= 512);

                // Here we only need to return a SIMD type with the same size as `ty`.
                // We use these types for spills and reloads, so prefer types with lanes <= 31
                // since that fits in the immediate field of `vsetivli`.
                const SIMD_TYPES: [[Type; 1]; 6] = [
                    [types::I8X2],
                    [types::I8X4],
                    [types::I8X8],
                    [types::I8X16],
                    [types::I16X16],
                    [types::I32X16],
                ];
                let idx = (ty.bytes().ilog2() - 1) as usize;
                let ty = &SIMD_TYPES[idx][..];

                Ok((&[RegClass::Vector], ty))
            }
            _ => Err(CodegenError::Unsupported(format!(
                "Unexpected SSA-value type: {}",
                ty
            ))),
        }
    }

    fn gen_jump(target: MachLabel) -> Inst {
        todo!()
    }

    fn worst_case_size() -> CodeOffset {
        // `BrIf { c, taken, not_taken }` expands to `br_if c, taken; jump not_taken`.
        //
        // The first instruction is six bytes long:
        //   * 1 byte opcode
        //   * 1 byte `c` register encoding
        //   * 4 byte `taken` displacement
        //
        // And the second instruction is five bytes long:
        //   * 1 byte opcode
        //   * 4 byte `not_taken` displacement
        11
    }

    fn ref_type_regclass(_settings: &settings::Flags) -> RegClass {
        RegClass::Int
    }

    fn function_alignment() -> FunctionAlignment {
        FunctionAlignment {
            minimum: 2,
            preferred: 2,
        }
    }
}

//=============================================================================
// Pretty-printing of instructions.

pub fn reg_name(reg: Reg) -> String {
    match reg.to_real_reg() {
        Some(real) => {
            let n = real.hw_enc();
            match (real.class(), n) {
                (RegClass::Int, 63) => format!("sp"),
                (RegClass::Int, 62) => format!("lr"),
                (RegClass::Int, 61) => format!("fp"),
                (RegClass::Int, 60) => format!("tmp0"),
                (RegClass::Int, 59) => format!("tmp1"),

                (RegClass::Int, _) => format!("x{n}"),
                (RegClass::Float, _) => format!("f{n}"),
                (RegClass::Vector, _) => format!("v{n}"),
            }
        }
        None => {
            format!("{:?}", reg)
        }
    }
}

impl Inst {
    fn print_with_state(&self, _state: &mut EmitState, allocs: &mut AllocationConsumer) -> String {
        use core::fmt::Write;

        let format_reg = |reg: Reg, _allocs: &mut AllocationConsumer| -> String { reg_name(reg) };

        let format_regs = |regs: &[Reg], allocs: &mut AllocationConsumer| -> String {
            let mut x = if regs.len() > 1 {
                String::from("[")
            } else {
                String::default()
            };
            regs.iter().for_each(|i| {
                x.push_str(format_reg(i.clone(), allocs).as_str());
                if *i != *regs.last().unwrap() {
                    x.push_str(",");
                }
            });
            if regs.len() > 1 {
                x.push_str("]");
            }
            x
        };

        let format_ext = |ext: ExtKind| -> &'static str {
            match ext {
                ExtKind::None => "",
                ExtKind::Sign => "_s",
                ExtKind::Zero => "_u",
            }
        };

        let format_labels = |labels: &[MachLabel]| -> String {
            if labels.len() == 0 {
                return String::from("[_]");
            }
            let mut x = String::from("[");
            labels.iter().for_each(|l| {
                x.push_str(
                    format!(
                        "{:?}{}",
                        l,
                        if l != labels.last().unwrap() { "," } else { "" },
                    )
                    .as_str(),
                );
            });
            x.push_str("]");
            x
        };

        let mut empty_allocs = AllocationConsumer::default();
        match self {
            Inst::Args { args } => {
                let mut s = "args".to_string();
                for arg in args {
                    let preg = format_reg(arg.preg, &mut empty_allocs);
                    let def = format_reg(arg.vreg.to_reg(), allocs);
                    write!(&mut s, " {}={}", def, preg).unwrap();
                }
                s
            }
            Inst::Rets { rets } => {
                let mut s = "rets".to_string();
                for ret in rets {
                    let preg = format_reg(ret.preg, &mut empty_allocs);
                    let vreg = format_reg(ret.vreg, allocs);
                    write!(&mut s, " {vreg}={preg}").unwrap();
                }
                s
            }

            Inst::Unwind { inst } => format!("unwind {inst:?}"),

            Inst::Trap { code } => format!("trap // code = {code:?}"),

            Inst::Nop => format!("nop"),

            Inst::Ret => format!("ret"),

            Inst::GetSp { dst } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = get_sp")
            }

            Inst::LoadExtName { dst, name, offset } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = load_ext_name {name:?}, {offset}")
            }

            Inst::Call { callee, info } => {
                format!("call {callee:?}, {info:?}")
            }

            Inst::IndirectCall { callee, info } => {
                let callee = format_reg(**callee, allocs);
                format!("indirect_call {callee}, {info:?}")
            }

            Inst::Jump { label } => format!("jump {}", label.to_string()),

            Inst::BrIf {
                c,
                taken,
                not_taken,
            } => {
                let c = format_reg(**c, allocs);
                let taken = taken.to_string();
                let not_taken = not_taken.to_string();
                format!("br_if {c}, {taken}; jump {not_taken}")
            }

            Inst::Xmov { dst, src } => todo!(),
            Inst::Fmov { dst, src } => todo!(),
            Inst::Vmov { dst, src } => todo!(),

            Inst::Xconst8 { dst, imm } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = xconst8 {imm}")
            }
            Inst::Xconst16 { dst, imm } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = xconst16 {imm}")
            }
            Inst::Xconst32 { dst, imm } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = xconst32 {imm}")
            }
            Inst::Xconst64 { dst, imm } => {
                let dst = format_reg(*dst.to_reg(), allocs);
                format!("{dst} = xconst64 {imm}")
            }

            Inst::Xadd32 { dst, src1, src2 } => format!(
                "{} = xadd32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xadd64 { .. } => todo!(),

            Inst::Xeq64 { dst, src1, src2 } => format!(
                "{} = xeq64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xneq64 { dst, src1, src2 } => format!(
                "{} = xneq64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xslt64 { dst, src1, src2 } => format!(
                "{} = xslt64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xslteq64 { dst, src1, src2 } => format!(
                "{} = xslteq64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xult64 { dst, src1, src2 } => format!(
                "{} = xult64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xulteq64 { dst, src1, src2 } => format!(
                "{} = xulteq64 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xeq32 { dst, src1, src2 } => format!(
                "{} = xeq32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xneq32 { dst, src1, src2 } => format!(
                "{} = xneq32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xslt32 { dst, src1, src2 } => format!(
                "{} = xslt32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xslteq32 { dst, src1, src2 } => format!(
                "{} = xslteq32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xult32 { dst, src1, src2 } => format!(
                "{} = xult32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),
            Inst::Xulteq32 { dst, src1, src2 } => format!(
                "{} = xulteq32 {}, {}",
                format_reg(*dst.to_reg(), allocs),
                format_reg(**src1, allocs),
                format_reg(**src2, allocs)
            ),

            Inst::Load {
                dst,
                mem,
                ty,
                flags,
                ext,
            } => {
                let dst = format_reg(dst.to_reg(), allocs);
                let ty = ty.bits();
                let ext = format_ext(*ext);
                let mem = mem.to_string();
                format!("{dst} = load{ty}{ext} {mem} // flags ={flags}")
            }

            Inst::Store {
                mem,
                src,
                ty,
                flags,
            } => {
                let ty = ty.bits();
                let mem = mem.to_string();
                let src = format_reg(*src, allocs);
                format!("store{ty} {mem}, {src} // flags = {flags}")
            }

            Inst::BitcastIntFromFloat32 { dst, src } => todo!(),
            Inst::BitcastIntFromFloat64 { dst, src } => todo!(),
            Inst::BitcastFloatFromInt32 { dst, src } => todo!(),
            Inst::BitcastFloatFromInt64 { dst, src } => todo!(),
        }
    }
}

/// Different forms of label references for different instruction formats.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LabelUse {
    /// A PC-relative `jump`/`call`/etc... instruction with an `i32` relative
    /// target. The payload value is an addend that describes the positive
    /// offset from the start of the instruction to the offset being relocated.
    Jump(u32),
}

impl MachInstLabelUse for LabelUse {
    /// Alignment for veneer code. Every pbc64 instruction must be
    /// 2-byte-aligned.
    const ALIGN: CodeOffset = 2;

    /// Maximum PC-relative range (positive), inclusive.
    fn max_pos_range(self) -> CodeOffset {
        match self {
            Self::Jump(_) => 0x7fff_ffff,
        }
    }

    /// Maximum PC-relative range (negative).
    fn max_neg_range(self) -> CodeOffset {
        match self {
            Self::Jump(_) => 0x8000_0000,
        }
    }

    /// Size of window into code needed to do the patch.
    fn patch_size(self) -> CodeOffset {
        match self {
            Self::Jump(_) => 4,
        }
    }

    /// Perform the patch.
    fn patch(self, buffer: &mut [u8], use_offset: CodeOffset, label_offset: CodeOffset) {
        let use_relative = (label_offset as i64) - (use_offset as i64);
        debug_assert!(use_relative <= self.max_pos_range() as i64);
        debug_assert!(use_relative >= -(self.max_neg_range() as i64));
        let pc_rel = i32::try_from(use_relative).unwrap() as u32;
        match self {
            Self::Jump(addend) => {
                let value = pc_rel.wrapping_add(addend);
                trace!(
                    "patching label use @ {use_offset:#x} to label {label_offset:#x} via \
                     PC-relative offset {pc_rel:#x}"
                );
                buffer.copy_from_slice(&value.to_le_bytes()[..]);
            }
        }
    }

    /// Is a veneer supported for this label reference type?
    fn supports_veneer(self) -> bool {
        match self {
            Self::Jump(_) => false,
        }
    }

    /// How large is the veneer, if supported?
    fn veneer_size(self) -> CodeOffset {
        match self {
            Self::Jump(_) => 0,
        }
    }

    fn worst_case_veneer_size() -> CodeOffset {
        0
    }

    /// Generate a veneer into the buffer, given that this veneer is at `veneer_offset`, and return
    /// an offset and label-use for the veneer's use of the original label.
    fn generate_veneer(
        self,
        buffer: &mut [u8],
        veneer_offset: CodeOffset,
    ) -> (CodeOffset, LabelUse) {
        match self {
            Self::Jump(_) => panic!("veneer not supported for {self:?}"),
        }
    }

    fn from_reloc(reloc: Reloc, addend: Addend) -> Option<LabelUse> {
        match reloc {
            Reloc::X86CallPCRel4 if addend < 0 => {
                // We are always relocating some offset that is within an
                // instruction, but pulley adds the offset relative to the PC
                // pointing to the *start* of the instruction. Therefore, adjust
                // back to the beginning of the instruction.
                Some(LabelUse::Jump(i32::try_from(-addend).unwrap() as u32))
            }
            _ => None,
        }
    }
}
