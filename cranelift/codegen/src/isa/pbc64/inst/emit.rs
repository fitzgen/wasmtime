//! Pbc64 ISA: binary code emission.

use crate::binemit::StackMap;
use crate::ir;
use crate::isa::pbc64::inst::*;
use crate::trace;
use cranelift_control::ControlPlane;
use cranelift_pulley::encode as enc;
use regalloc2::Allocation;

pub struct EmitInfo {
    shared_flags: settings::Flags,
    isa_flags: super::super::pbc64_settings::Flags,
}

impl EmitInfo {
    pub(crate) fn new(
        shared_flags: settings::Flags,
        isa_flags: super::super::pbc64_settings::Flags,
    ) -> Self {
        Self {
            shared_flags,
            isa_flags,
        }
    }
}

/// State carried between emissions of a sequence of instructions.
#[derive(Default, Clone, Debug)]
pub struct EmitState {
    ctrl_plane: ControlPlane,
    stack_map: Option<StackMap>,
    pub virtual_sp_offset: i64,
    pub nominal_sp_to_fp: i64,
    frame_layout: FrameLayout,
}

impl EmitState {
    fn take_stack_map(&mut self) -> Option<StackMap> {
        self.stack_map.take()
    }

    pub fn frame_layout(&self) -> &FrameLayout {
        &self.frame_layout
    }
}

impl MachInstEmitState<Inst> for EmitState {
    fn new(
        abi: &Callee<crate::isa::pbc64::abi::Pbc64MachineDeps>,
        ctrl_plane: ControlPlane,
    ) -> Self {
        EmitState {
            ctrl_plane,
            stack_map: None,
            virtual_sp_offset: 0,
            nominal_sp_to_fp: 0,
            frame_layout: abi.frame_layout().clone(),
        }
    }

    fn pre_safepoint(&mut self, stack_map: StackMap) {
        self.stack_map = Some(stack_map);
    }

    fn ctrl_plane_mut(&mut self) -> &mut ControlPlane {
        &mut self.ctrl_plane
    }

    fn take_ctrl_plane(self) -> ControlPlane {
        self.ctrl_plane
    }

    fn on_new_block(&mut self) {}
}

impl MachInstEmit for Inst {
    type State = EmitState;
    type Info = EmitInfo;

    fn emit(
        &self,
        _allocs: &[Allocation],
        sink: &mut MachBuffer<Inst>,
        emit_info: &Self::Info,
        state: &mut EmitState,
    ) {
        // N.B.: we *must* not exceed the "worst-case size" used to compute
        // where to insert islands, except when islands are explicitly triggered
        // (with an `EmitIsland`). We check this in debug builds. This is `mut`
        // to allow disabling the check for `JTSequence`, which is always
        // emitted following an `EmitIsland`.
        let start = sink.cur_offset();
        pbc64_emit(self, sink, emit_info, state, start);

        let end = sink.cur_offset();
        assert!(
            (end - start) <= Inst::worst_case_size(),
            "encoded inst {self:?} longer than worst-case size: length: {}, Inst::worst_case_size() = {}",
            end - start,
            Inst::worst_case_size()
        );
    }

    fn pretty_print_inst(&self, allocs: &[Allocation], state: &mut Self::State) -> String {
        let mut allocs = AllocationConsumer::new(allocs);
        self.print_with_state(state, &mut allocs)
    }
}

const LABEL_PLACEHOLDER: i32 = 0x42424242;

fn pbc64_emit(
    inst: &Inst,
    sink: &mut MachBuffer<Inst>,
    emit_info: &EmitInfo,
    state: &mut EmitState,
    start_offset: u32,
) {
    let mut data: SmallVec<[_; 16]> = smallvec![];
    match inst {
        // Pseduo-instructions that don't actually encode to anything.
        Inst::Args { .. } | Inst::Rets { .. } | Inst::Unwind { .. } => {}

        Inst::Trap { code } => {
            sink.add_trap(*code);
            enc::trap(&mut data);
        }

        Inst::Nop => todo!(),

        Inst::Ret => enc::ret(&mut data),

        Inst::Jump { label } => {
            sink.use_label_at_offset(start_offset + 1, *label, LabelUse::Jump);
            sink.add_uncond_branch(start_offset, start_offset + 5, *label);
            enc::jump(&mut data, LABEL_PLACEHOLDER);
        }

        Inst::BrIf {
            c,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 1;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump);
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_not(&mut inverted, c.into(), LABEL_PLACEHOLDER);
            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if(&mut data, c.into(), LABEL_PLACEHOLDER);

            // If not taken.
            let not_taken_start = sink.cur_offset() + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump);
            sink.add_uncond_branch(sink.cur_offset(), not_taken_end, *not_taken);
            enc::jump(&mut data, LABEL_PLACEHOLDER);
        }

        Inst::Xmov { dst, src } => enc::xmov(&mut data, dst.into(), src.into()),
        Inst::Fmov { dst, src } => todo!(),
        Inst::Vmov { dst, src } => todo!(),

        Inst::Xconst8 { dst, imm } => enc::xconst8(&mut data, dst.into(), *imm),
        Inst::Xconst16 { dst, imm } => enc::xconst16(&mut data, dst.into(), *imm),
        Inst::Xconst32 { dst, imm } => enc::xconst32(&mut data, dst.into(), *imm),
        Inst::Xconst64 { dst, imm } => enc::xconst64(&mut data, dst.into(), *imm),

        Inst::Xadd32 { dst, src1, src2 } => {
            enc::xadd32(&mut data, dst.into(), src1.into(), src2.into())
        }
        Inst::Xadd64 { .. } => todo!(),

        Inst::Load {
            dst,
            mem,
            ty,
            flags,
            ext,
        } => {
            use ExtKind as X;
            let r = mem.get_base_register().unwrap();
            let r = cranelift_pulley::regs::XReg::new(r.to_real_reg().unwrap().hw_enc()).unwrap();
            let dst =
                cranelift_pulley::regs::XReg::new(dst.to_reg().to_real_reg().unwrap().hw_enc())
                    .unwrap();
            let x = mem.get_offset_with_state(state);
            match (
                *ext,
                *ty,
                i8::try_from(x),
                i16::try_from(x),
                i32::try_from(x),
            ) {
                (X::Sign, types::I32, Ok(0), _, _) => enc::load32_s(&mut data, dst, r),
                (X::Sign, types::I32, Ok(x), _, _) => enc::load32_s_offset8(&mut data, dst, r, x),
                (X::Sign, types::I32, _, Ok(x), _) => enc::load32_s_offset16(&mut data, dst, r, x),
                (X::Sign, types::I32, _, _, Ok(x)) => enc::load32_s_offset32(&mut data, dst, r, x),
                (X::Sign, types::I32, _, _, _) => enc::load32_s_offset64(&mut data, dst, r, x),

                (X::Zero, types::I32, Ok(0), _, _) => enc::load32_u(&mut data, dst, r),
                (X::Zero, types::I32, Ok(x), _, _) => enc::load32_u_offset8(&mut data, dst, r, x),
                (X::Zero, types::I32, _, Ok(x), _) => enc::load32_u_offset16(&mut data, dst, r, x),
                (X::Zero, types::I32, _, _, Ok(x)) => enc::load32_u_offset32(&mut data, dst, r, x),
                (X::Zero, types::I32, _, _, _) => enc::load32_u_offset64(&mut data, dst, r, x),

                (_, types::I64, Ok(0), _, _) => enc::load64(&mut data, dst, r),
                (_, types::I64, Ok(x), _, _) => enc::load64_offset8(&mut data, dst, r, x),
                (_, types::I64, _, Ok(x), _) => enc::load64_offset16(&mut data, dst, r, x),
                (_, types::I64, _, _, Ok(x)) => enc::load64_offset32(&mut data, dst, r, x),
                (_, types::I64, _, _, _) => enc::load64_offset64(&mut data, dst, r, x),

                (..) => unimplemented!("load ext={ext:?} ty={ty}"),
            }
        }

        Inst::Store {
            mem,
            src,
            ty,
            flags,
        } => {
            let r = mem.get_base_register().unwrap();
            let r = cranelift_pulley::regs::XReg::new(r.to_real_reg().unwrap().hw_enc()).unwrap();
            let src =
                cranelift_pulley::regs::XReg::new(src.to_real_reg().unwrap().hw_enc()).unwrap();
            let x = mem.get_offset_with_state(state);
            match (*ty, i8::try_from(x), i16::try_from(x), i32::try_from(x)) {
                (types::I32, Ok(0), _, _) => enc::store32(&mut data, r, src),
                (types::I32, Ok(x), _, _) => enc::store32_offset8(&mut data, r, x, src),
                (types::I32, _, Ok(x), _) => enc::store32_offset16(&mut data, r, x, src),
                (types::I32, _, _, Ok(x)) => enc::store32_offset32(&mut data, r, x, src),
                (types::I32, _, _, _) => enc::store32_offset64(&mut data, r, x, src),

                (types::I64, Ok(0), _, _) => enc::store64(&mut data, r, src),
                (types::I64, Ok(x), _, _) => enc::store64_offset8(&mut data, r, x, src),
                (types::I64, _, Ok(x), _) => enc::store64_offset16(&mut data, r, x, src),
                (types::I64, _, _, Ok(x)) => enc::store64_offset32(&mut data, r, x, src),
                (types::I64, _, _, _) => enc::store64_offset64(&mut data, r, x, src),

                (..) => todo!(),
            }
        }

        Inst::BitcastIntFromFloat32 { dst, src } => todo!(),
        Inst::BitcastIntFromFloat64 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt32 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt64 { dst, src } => todo!(),
    }
    sink.put_data(&data);
}
