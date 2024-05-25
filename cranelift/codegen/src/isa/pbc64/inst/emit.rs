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

    pub(crate) fn adjust_virtual_sp_offset(&mut self, amount: i64) {
        let old = self.virtual_sp_offset;
        let new = self.virtual_sp_offset + amount;
        trace!("adjust virtual sp offset by {amount:#x}: {old:#x} -> {new:#x}",);
        self.virtual_sp_offset = new;
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

fn pbc64_emit(
    inst: &Inst,
    sink: &mut MachBuffer<Inst>,
    emit_info: &EmitInfo,
    state: &mut EmitState,
    start_offset: u32,
) {
    match inst {
        // Pseduo-instructions that don't actually encode to anything.
        Inst::Args { .. } | Inst::Rets { .. } | Inst::Unwind { .. } => {}

        Inst::Trap { code } => {
            sink.add_trap(*code);
            enc::trap(sink);
        }

        Inst::Nop => todo!(),

        Inst::GetSp { dst } => enc::get_sp(sink, dst),

        Inst::Ret => enc::ret(sink),

        Inst::LoadExtName { dst, name, offset } => todo!(),

        Inst::Call { callee, info } => {
            if let Some(s) = state.take_stack_map() {
                sink.add_stack_map(StackMapExtent::UpcomingBytes(5), s);
            }
            sink.put1(cranelift_pulley::Opcode::Call as u8);
            sink.add_reloc(
                // TODO: is it actually okay to reuse this reloc here?
                Reloc::X86CallPCRel4,
                &**callee,
                // This addend adjusts for the difference between the start of
                // the instruction and the beginning of the immediate field.
                -1,
            );
            sink.put4(0);
            if info.opcode.is_call() {
                sink.add_call_site(info.opcode);
            }

            let callee_pop_size = i64::from(info.callee_pop_size);
            state.adjust_virtual_sp_offset(-callee_pop_size);
        }

        Inst::IndirectCall { callee, info } => {
            todo!()
        }

        Inst::Jump { label } => {
            sink.use_label_at_offset(start_offset + 1, *label, LabelUse::Jump(1));
            sink.add_uncond_branch(start_offset, start_offset + 5, *label);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIf {
            c,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 2;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(2));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_not(&mut inverted, c, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if(sink, c, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXeq32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xneq32(&mut inverted, src1, src2, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xeq32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXneq32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xeq32(&mut inverted, src1, src2, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xneq32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXslt32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xslteq32(&mut inverted, src2, src1, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xslt32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXslteq32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xslt32(&mut inverted, src2, src1, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xslteq32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXult32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xulteq32(&mut inverted, src2, src1, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xult32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::BrIfXulteq32 {
            src1,
            src2,
            taken,
            not_taken,
        } => {
            // If taken.
            let taken_start = start_offset + 3;
            let taken_end = taken_start + 4;

            sink.use_label_at_offset(taken_start, *taken, LabelUse::Jump(3));
            let mut inverted = SmallVec::<[u8; 16]>::new();
            enc::br_if_xult32(&mut inverted, src2, src1, 0x00000000);
            debug_assert_eq!(
                inverted.len(),
                usize::try_from(taken_end - start_offset).unwrap()
            );

            sink.add_cond_branch(start_offset, taken_end, *taken, &inverted);
            enc::br_if_xulteq32(sink, src1, src2, 0x00000000);
            debug_assert_eq!(sink.cur_offset(), taken_end);

            // If not taken.
            let not_taken_start = taken_end + 1;
            let not_taken_end = not_taken_start + 4;

            sink.use_label_at_offset(not_taken_start, *not_taken, LabelUse::Jump(1));
            sink.add_uncond_branch(taken_end, not_taken_end, *not_taken);
            enc::jump(sink, 0x00000000);
        }

        Inst::Xmov { dst, src } => enc::xmov(sink, dst, src),
        Inst::Fmov { dst, src } => todo!(),
        Inst::Vmov { dst, src } => todo!(),

        Inst::Xconst8 { dst, imm } => enc::xconst8(sink, dst, *imm),
        Inst::Xconst16 { dst, imm } => enc::xconst16(sink, dst, *imm),
        Inst::Xconst32 { dst, imm } => enc::xconst32(sink, dst, *imm),
        Inst::Xconst64 { dst, imm } => enc::xconst64(sink, dst, *imm),

        Inst::Xadd32 { dst, src1, src2 } => {
            enc::xadd32(sink, dst, src1, src2);
        }
        Inst::Xadd64 { dst, src1, src2 } => {
            enc::xadd64(sink, dst, src1, src2);
        }

        Inst::Xeq64 { dst, src1, src2 } => {
            enc::xeq64(sink, dst, src1, src2);
        }
        Inst::Xneq64 { dst, src1, src2 } => {
            enc::xneq64(sink, dst, src1, src2);
        }
        Inst::Xslt64 { dst, src1, src2 } => {
            enc::xslt64(sink, dst, src1, src2);
        }
        Inst::Xslteq64 { dst, src1, src2 } => {
            enc::xslteq64(sink, dst, src1, src2);
        }
        Inst::Xult64 { dst, src1, src2 } => {
            enc::xult64(sink, dst, src1, src2);
        }
        Inst::Xulteq64 { dst, src1, src2 } => {
            enc::xulteq64(sink, dst, src1, src2);
        }
        Inst::Xeq32 { dst, src1, src2 } => {
            enc::xeq32(sink, dst, src1, src2);
        }
        Inst::Xneq32 { dst, src1, src2 } => {
            enc::xneq32(sink, dst, src1, src2);
        }
        Inst::Xslt32 { dst, src1, src2 } => {
            enc::xslt32(sink, dst, src1, src2);
        }
        Inst::Xslteq32 { dst, src1, src2 } => {
            enc::xslteq32(sink, dst, src1, src2);
        }
        Inst::Xult32 { dst, src1, src2 } => {
            enc::xult32(sink, dst, src1, src2);
        }
        Inst::Xulteq32 { dst, src1, src2 } => {
            enc::xulteq32(sink, dst, src1, src2);
        }

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
                (X::Sign, types::I32, Ok(0), _, _) => enc::load32_s(sink, dst, r),
                (X::Sign, types::I32, Ok(x), _, _) => enc::load32_s_offset8(sink, dst, r, x),
                (X::Sign, types::I32, _, Ok(x), _) => enc::load32_s_offset16(sink, dst, r, x),
                (X::Sign, types::I32, _, _, Ok(x)) => enc::load32_s_offset32(sink, dst, r, x),
                (X::Sign, types::I32, _, _, _) => enc::load32_s_offset64(sink, dst, r, x),

                (X::Zero, types::I32, Ok(0), _, _) => enc::load32_u(sink, dst, r),
                (X::Zero, types::I32, Ok(x), _, _) => enc::load32_u_offset8(sink, dst, r, x),
                (X::Zero, types::I32, _, Ok(x), _) => enc::load32_u_offset16(sink, dst, r, x),
                (X::Zero, types::I32, _, _, Ok(x)) => enc::load32_u_offset32(sink, dst, r, x),
                (X::Zero, types::I32, _, _, _) => enc::load32_u_offset64(sink, dst, r, x),

                (_, types::I64, Ok(0), _, _) => enc::load64(sink, dst, r),
                (_, types::I64, Ok(x), _, _) => enc::load64_offset8(sink, dst, r, x),
                (_, types::I64, _, Ok(x), _) => enc::load64_offset16(sink, dst, r, x),
                (_, types::I64, _, _, Ok(x)) => enc::load64_offset32(sink, dst, r, x),
                (_, types::I64, _, _, _) => enc::load64_offset64(sink, dst, r, x),

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
                (types::I32, Ok(0), _, _) => enc::store32(sink, r, src),
                (types::I32, Ok(x), _, _) => enc::store32_offset8(sink, r, x, src),
                (types::I32, _, Ok(x), _) => enc::store32_offset16(sink, r, x, src),
                (types::I32, _, _, Ok(x)) => enc::store32_offset32(sink, r, x, src),
                (types::I32, _, _, _) => enc::store32_offset64(sink, r, x, src),

                (types::I64, Ok(0), _, _) => enc::store64(sink, r, src),
                (types::I64, Ok(x), _, _) => enc::store64_offset8(sink, r, x, src),
                (types::I64, _, Ok(x), _) => enc::store64_offset16(sink, r, x, src),
                (types::I64, _, _, Ok(x)) => enc::store64_offset32(sink, r, x, src),
                (types::I64, _, _, _) => enc::store64_offset64(sink, r, x, src),

                (..) => todo!(),
            }
        }

        Inst::BitcastIntFromFloat32 { dst, src } => todo!(),
        Inst::BitcastIntFromFloat64 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt32 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt64 { dst, src } => todo!(),
    }
}
