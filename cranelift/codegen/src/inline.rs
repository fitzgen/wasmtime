//! TODO FITZGEN

use crate::cursor::{Cursor as _, FuncCursor};
use crate::ir;
use crate::result::CodegenResult;

/// TODO FITZGEN
pub enum InlineCommand<'a> {
    /// TODO FITZGEN
    Call,
    /// TODO FITZGEN
    Inline(&'a ir::Function),
}

/// TODO FITZGEN
pub trait Inline {
    /// TODO FITZGEN
    fn inline(
        &self,
        caller: &ir::Function,
        inst: ir::Inst,
        opcode: ir::Opcode,
        callee: ir::FuncRef,
        args: &[ir::Value],
    ) -> InlineCommand<'_>;
}

#[allow(warnings)] // TODO FITZGEN
pub(crate) fn do_inlining(func: &mut ir::Function, inliner: &impl Inline) -> CodegenResult<bool> {
    let mut cursor = FuncCursor::new(func);
    while let Some(block) = cursor.next_block() {
        let mut prev_pos;
        while let Some(inst) = {
            prev_pos = cursor.position();
            cursor.next_inst()
        } {
            match cursor.func.dfg.insts[inst] {
                ir::InstructionData::Call {
                    opcode: opcode @ ir::Opcode::Call | opcode @ ir::Opcode::ReturnCall,
                    args,
                    func_ref,
                } => {
                    let args = cursor.func.dfg.inst_args(inst);
                    match inliner.inline(&cursor.func, inst, opcode, func_ref, args) {
                        InlineCommand::Call => continue,
                        InlineCommand::Inline(callee) => {
                            todo!();
                            cursor.set_position(prev_pos);
                        }
                    }
                }
                ir::InstructionData::TryCall {
                    opcode: opcode @ ir::Opcode::TryCall,
                    args,
                    func_ref,
                    exception,
                } => {
                    let args = cursor.func.dfg.inst_args(inst);
                    match inliner.inline(&cursor.func, inst, opcode, func_ref, args) {
                        InlineCommand::Call => continue,
                        InlineCommand::Inline(callee) => {
                            todo!();
                            cursor.set_position(prev_pos);
                        }
                    }
                }
                _ => continue,
            }
        }
    }
    todo!()
}
