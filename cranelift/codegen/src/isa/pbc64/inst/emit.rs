//! Pbc64 ISA: binary code emission.

use crate::binemit::StackMap;
use crate::ir;
use crate::isa::pbc64::inst::*;
use crate::trace;
use cranelift_control::ControlPlane;
use cranelift_pulley::encode;
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
            "Inst:{:?} length:{} worst_case_size:{}",
            self,
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
    let mut data: SmallVec<[_; 16]> = smallvec![];
    match inst {
        // Pseduo-instructions that don't actually encode to anything.
        Inst::Args { .. } | Inst::Rets { .. } | Inst::Unwind { .. } => {}

        Inst::Trap => todo!(),

        Inst::Nop => todo!(),

        Inst::Ret => encode::ret(&mut data),

        Inst::Jump { label } => {
            sink.use_label_at_offset(start_offset, *label, LabelUse::Jump);
            sink.add_uncond_branch(start_offset, start_offset + 5, *label);
            encode::jump(&mut data, 0);
        }

        Inst::Xmov { dst, src } => todo!(),
        Inst::Fmov { dst, src } => todo!(),
        Inst::Vmov { dst, src } => todo!(),

        Inst::Xconst8 { dst, imm } => todo!(),
        Inst::Xconst16 { dst, imm } => todo!(),
        Inst::Xconst32 { dst, imm } => todo!(),
        Inst::Xconst64 { dst, imm } => todo!(),

        Inst::Xadd32 { dst, src1, src2 } => {
            encode::xadd32(&mut data, dst.into(), src1.into(), src2.into())
        }
        Inst::Xadd64 { .. } => todo!(),

        Inst::Load { .. } => todo!(),

        Inst::BitcastIntFromFloat32 { dst, src } => todo!(),
        Inst::BitcastIntFromFloat64 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt32 { dst, src } => todo!(),
        Inst::BitcastFloatFromInt64 { dst, src } => todo!(),
    }
    sink.put_data(&data);
}
