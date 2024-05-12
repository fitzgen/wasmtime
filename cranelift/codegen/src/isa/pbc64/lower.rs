//! Lowering rules for Pbc64.

pub mod isle;

use crate::{
    ir,
    isa::pbc64::{inst::*, Pbc64Backend},
    machinst::{lower::*, *},
};

impl LowerBackend for Pbc64Backend {
    type MInst = Inst;

    fn lower(&self, ctx: &mut Lower<Inst>, ir_inst: ir::Inst) -> Option<InstOutput> {
        isle::lower(ctx, self, ir_inst)
    }

    fn lower_branch(
        &self,
        ctx: &mut Lower<Inst>,
        ir_inst: ir::Inst,
        targets: &[MachLabel],
    ) -> Option<()> {
        isle::lower_branch(ctx, self, ir_inst, targets)
    }

    fn maybe_pinned_reg(&self) -> Option<Reg> {
        // pinned register is a register that you want put anything in it.
        // right now pbc64 not support this feature.
        None
    }

    type FactFlowState = ();
}
