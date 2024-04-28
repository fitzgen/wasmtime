//! Support compiling to 64-bit pulley bytecode (pbc64).

#![allow(warnings)] // TODO FITZGEN

mod abi;
mod inst;
mod lower;
mod settings;

use self::{inst::EmitInfo, settings as pbc64_settings};
use super::{Builder as IsaBuilder, FunctionAlignment};
use crate::{
    dominator_tree::DominatorTree,
    ir,
    isa::{self, OwnedTargetIsa, TargetIsa},
    machinst::{self, CompiledCodeStencil, MachInst, SigSet, VCode},
    result::CodegenResult,
    settings::{self as shared_settings, Flags},
    MachTextSectionBuilder, TextSectionBuilder,
};
use alloc::boxed::Box;
use alloc::vec::Vec;
use cranelift_control::ControlPlane;
use target_lexicon::{Architecture, Triple};

/// TODO FITZGEN
pub struct Pbc64Backend {
    triple: Triple,
    flags: Flags,
    isa_flags: pbc64_settings::Flags,
}

impl core::fmt::Debug for Pbc64Backend {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let Pbc64Backend {
            triple,
            flags: _,
            isa_flags: _,
        } = self;
        f.debug_struct("Pbc64Backend")
            .field("triple", &self.triple)
            .finish_non_exhaustive()
    }
}

impl core::fmt::Display for Pbc64Backend {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}

impl Pbc64Backend {
    /// Create a new pbc64 backend with the given (shared) flags.
    pub fn new_with_flags(
        triple: Triple,
        flags: shared_settings::Flags,
        isa_flags: pbc64_settings::Flags,
    ) -> Pbc64Backend {
        Pbc64Backend {
            triple,
            flags,
            isa_flags,
        }
    }

    /// This performs lowering to VCode, register-allocates the code, computes block layout and
    /// finalizes branches. The result is ready for binary emission.
    fn compile_vcode(
        &self,
        func: &ir::Function,
        domtree: &DominatorTree,
        ctrl_plane: &mut ControlPlane,
    ) -> CodegenResult<(VCode<inst::Inst>, regalloc2::Output)> {
        let emit_info = EmitInfo::new(self.flags.clone(), self.isa_flags.clone());
        let sigs = SigSet::new::<abi::Pbc64MachineDeps>(func, &self.flags)?;
        let abi = abi::Pbc64Callee::new(func, self, &self.isa_flags, &sigs)?;
        machinst::compile::compile::<Pbc64Backend>(
            func, domtree, self, abi, emit_info, sigs, ctrl_plane,
        )
    }
}

impl TargetIsa for Pbc64Backend {
    fn name(&self) -> &'static str {
        "pbc64"
    }

    fn triple(&self) -> &Triple {
        &self.triple
    }

    fn flags(&self) -> &Flags {
        &self.flags
    }

    fn isa_flags(&self) -> Vec<shared_settings::Value> {
        self.isa_flags.iter().collect()
    }

    fn dynamic_vector_bytes(&self, _dynamic_ty: ir::Type) -> u32 {
        512
    }

    fn compile_function(
        &self,
        func: &ir::Function,
        domtree: &DominatorTree,
        want_disasm: bool,
        ctrl_plane: &mut cranelift_control::ControlPlane,
    ) -> CodegenResult<CompiledCodeStencil> {
        let (vcode, regalloc_result) = self.compile_vcode(func, domtree, ctrl_plane)?;

        let want_disasm = want_disasm || log::log_enabled!(log::Level::Debug);
        let emit_result = vcode.emit(&regalloc_result, want_disasm, &self.flags, ctrl_plane);
        let frame_size = emit_result.frame_size;
        let value_labels_ranges = emit_result.value_labels_ranges;
        let buffer = emit_result.buffer;
        let sized_stackslot_offsets = emit_result.sized_stackslot_offsets;
        let dynamic_stackslot_offsets = emit_result.dynamic_stackslot_offsets;

        if let Some(disasm) = emit_result.disasm.as_ref() {
            log::debug!("disassembly:\n{}", disasm);
        }

        let mut disas = cranelift_pulley::disas::Disassembler::new(&buffer.data);
        cranelift_pulley::decode::Decoder::decode_all(&mut disas, &buffer.data);
        log::debug!("FITZGEN: pulley disassembly:\n{}", disas.disas());

        Ok(CompiledCodeStencil {
            buffer,
            frame_size,
            vcode: emit_result.disasm,
            value_labels_ranges,
            sized_stackslot_offsets,
            dynamic_stackslot_offsets,
            bb_starts: emit_result.bb_offsets,
            bb_edges: emit_result.bb_edges,
        })
    }

    fn emit_unwind_info(
        &self,
        result: &crate::CompiledCode,
        kind: super::unwind::UnwindInfoKind,
    ) -> CodegenResult<Option<isa::unwind::UnwindInfo>> {
        // TODO FITZGEN
        Ok(None)
    }

    fn text_section_builder(
        &self,
        num_labeled_funcs: usize,
    ) -> alloc::boxed::Box<dyn TextSectionBuilder> {
        Box::new(MachTextSectionBuilder::<inst::Inst>::new(num_labeled_funcs))
    }

    fn function_alignment(&self) -> FunctionAlignment {
        inst::Inst::function_alignment()
    }

    fn has_native_fma(&self) -> bool {
        false
    }

    fn has_x86_blendv_lowering(&self, _ty: ir::Type) -> bool {
        false
    }

    fn has_x86_pshufb_lowering(&self) -> bool {
        false
    }

    fn has_x86_pmulhrsw_lowering(&self) -> bool {
        false
    }

    fn has_x86_pmaddubsw_lowering(&self) -> bool {
        false
    }
}

/// TODO FITZGEN
pub fn isa_builder(triple: Triple) -> IsaBuilder {
    assert!(matches!(triple.architecture, Architecture::Pbc64));
    IsaBuilder {
        triple,
        setup: self::settings::builder(),
        constructor: isa_constructor,
    }
}

fn isa_constructor(
    triple: Triple,
    shared_flags: Flags,
    builder: &shared_settings::Builder,
) -> CodegenResult<OwnedTargetIsa> {
    let isa_flags = pbc64_settings::Flags::new(&shared_flags, builder);
    let backend = Pbc64Backend::new_with_flags(triple, shared_flags, isa_flags);
    Ok(backend.wrapped())
}
