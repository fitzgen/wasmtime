//! TODO FITZGEN

use mutatis::{mutators as m, Context, DefaultMutate, Generate, Mutate};
use wasmtime_environ::{
    EngineOrModuleTypeIndex, ModuleInternedTypeIndex, WasmArrayType, WasmCompositeType,
    WasmFieldType, WasmHeapType, WasmRefType, WasmStorageType, WasmStructType, WasmSubType,
    WasmValType,
};

/// TODO FITZGEN
#[derive(Mutate)]
#[mutatis(default_mutate = false)]
pub struct GcOps {
    types: Vec<WasmSubType>,
    ops: Vec<GcExpr>,
}

impl DefaultMutate for GcOps {
    type DefaultMutate = GcOpsMutator<m::Vec<TypesMutator>, GcExprMutator>;
}

impl GcOps {
    /// TODO FITZGEN
    pub fn to_wasm(&self) -> Vec<u8> {
        todo!()
    }
}

/// TODO FITZGEN
pub enum GcExpr {
    /// Call an imported function to run a GC.
    Gc,

    /// Call an imported function to get an `externref`.
    AllocExternRef,

    /// A `local.get` instruction.
    LocalGet(u32),

    /// A `local.set` instruction.
    LocalSet(u32),

    /// A `local.tee` instruction.
    LocalTee(u32),

    /// TODO FITZGEN
    GlobalGet(u32),

    /// TODO FITZGEN
    GlobalSet(u32),

    /// A `table.get` instruction.
    TableGet,

    /// A `table.set` instruction.
    TableSet,

    /// TODO FITZGEN
    RefNull(u32),

    /// A `struct.new` instruction.
    StructNew(u32),

    /// A `struct.new_default` instruction.
    StructNewDefault(u32),

    /// TODO FITZGEN
    StructGet(u32, u32),

    /// TODO FITZGEN
    StructGetS(u32, u32),

    /// TODO FITZGEN
    StructGetU(u32, u32),

    /// TODO FITZGEN
    StructSet(u32, u32),
    // TODO: array.new

    // TODO: array.new_default

    // TODO: array.new_fixed

    // TODO: array.new_data

    // TODO: array.new_elem

    // TODO: array.copy

    // TODO: array.fill

    // TODO: array.init_data

    // TODO: array.init_elem

    // TODO: array.len

    // TODO: array.get

    // TODO: array.get_s

    // TODO: array.get_u

    // TODO: array.set

    // TODO: any.convert_extern

    // TODO: extern.convert_any

    // TODO: ref.eq

    // TODO: ref.test

    // TODO: ref.cast

    // TODO: ref.as_non_null

    // TODO: br_on_cast

    // TODO: br_on_cast_fail

    // TODO: br_on_null

    // TODO: br_on_non_null

    // TODO: ref.i31

    // TODO: i31.get_s

    // TODO: i31.get_u
}

pub struct GcExprMutator;

impl DefaultMutate for GcExpr {
    type DefaultMutate = GcExprMutator;
}

impl Mutate<Vec<GcExpr>> for GcExprMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        exprs: &mut Vec<GcExpr>,
    ) -> mutatis::Result<()> {
        // Insert a new stack-neutral expression.
        c.mutation(|ctx| todo!())?;

        // Replace an expression with its default value.
        c.mutation(|ctx| todo!())?;

        // Remove an expression that doesn't produce a result.
        c.mutation(|ctx| todo!())?;

        // Change this expression to any other equivalently-typed expression.
        c.mutation(|ctx| todo!())?;

        match expr {
            GcExpr::Gc => {}
            GcExpr::AllocExternRef => {}
            GcExpr::LocalGet(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::LocalSet(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::LocalTee(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::GlobalGet(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::GlobalSet(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::TableGet => {}
            GcExpr::TableSet => {}
            GcExpr::RefNull(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::StructNew(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::StructNewDefault(idx) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::StructGet(idx, field) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    *field = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::StructGetS(idx, field) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    *field = ctx.rng().gen_u
                })?;
            }
            GcExpr::StructGetU(idx, field) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    *field = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
            GcExpr::StructSet(idx, field) => {
                c.mutation(|ctx| {
                    *idx = ctx.rng().gen_u32();
                    *field = ctx.rng().gen_u32();
                    Ok(())
                })?;
            }
        }
        Ok(())
    }
}

// TODO FITZGEN: should have `derive(Mutate)` also implement `Generate` for
// `GcOpMutator`.
impl Generate<GcOp> for GcOpMutator {
    fn generate(&mut self, ctx: &mut Context) -> mutatis::Result<GcOp> {
        let choices: &[fn(&mut Context) -> GcOp] = &[
            |_ctx| GcOp::Gc,
            |_ctx| GcOp::AllocExternRef,
            |ctx| GcOp::LocalGet(ctx.rng().gen_u32()),
            |ctx| GcOp::LocalSet(ctx.rng().gen_u32()),
            |ctx| GcOp::LocalTee(ctx.rng().gen_u32()),
            |ctx| GcOp::GlobalGet(ctx.rng().gen_u32()),
            |ctx| GcOp::GlobalSet(ctx.rng().gen_u32()),
            |_ctx| GcOp::TableGet,
            |_ctx| GcOp::TableSet,
            |ctx| GcOp::RefNull(ctx.rng().gen_u32()),
            |ctx| GcOp::StructNew(ctx.rng().gen_u32()),
            |ctx| GcOp::StructNewDefault(ctx.rng().gen_u32()),
            |ctx| GcOp::StructGet(ctx.rng().gen_u32(), ctx.rng().gen_u32()),
            |ctx| GcOp::StructGetS(ctx.rng().gen_u32(), ctx.rng().gen_u32()),
            |ctx| GcOp::StructGetU(ctx.rng().gen_u32(), ctx.rng().gen_u32()),
            |ctx| GcOp::StructSet(ctx.rng().gen_u32(), ctx.rng().gen_u32()),
        ];

        let f = ctx.rng().choose(choices).unwrap();
        Ok(f(ctx))
    }
}

/// TODO FITZGEN
#[derive(Default)]
pub struct TypesMutator;

impl Generate<WasmSubType> for TypesMutator {
    fn generate(&mut self, _: &mut Context) -> mutatis::Result<WasmSubType> {
        Ok(WasmSubType {
            is_final: false,
            supertype: None,
            composite_type: WasmCompositeType::Struct(WasmStructType {
                fields: vec![].into_boxed_slice(),
            }),
        })
    }
}

impl Generate<WasmFieldType> for TypesMutator {
    fn generate(&mut self, _: &mut Context) -> mutatis::Result<WasmFieldType> {
        Ok(WasmFieldType {
            element_type: WasmStorageType::I8,
            mutable: true,
        })
    }
}

impl Mutate<WasmSubType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmSubType,
    ) -> mutatis::Result<()> {
        // Finality.
        c.mutation(|_ctx| {
            ty.is_final = !ty.is_final;
            Ok(())
        })?;

        // Add a supertype.
        if ty.supertype.is_none() && !c.shrink() {
            c.mutation(|ctx| {
                ty.supertype = Some(EngineOrModuleTypeIndex::Module(
                    ModuleInternedTypeIndex::from_bits(ctx.rng().gen_u32()),
                ));
                Ok(())
            })?;
        }

        // Remove a supertype.
        if ty.supertype.is_some() {
            c.mutation(|_ctx| {
                ty.supertype = None;
                Ok(())
            })?;
        }

        // Mutate an existing supertype.
        if let Some(sup) = &mut ty.supertype {
            self.mutate(c, sup)?;
        }

        self.mutate(c, &mut ty.composite_type)
    }
}

impl Mutate<WasmCompositeType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmCompositeType,
    ) -> mutatis::Result<()> {
        // Convert to an array.
        if !ty.is_array() {
            c.mutation(|_| {
                *ty = WasmCompositeType::Array(WasmArrayType(WasmFieldType {
                    element_type: WasmStorageType::I8,
                    mutable: true,
                }));
                Ok(())
            })?;
        }

        // Convert to a struct.
        if !ty.is_struct() {
            c.mutation(|_| {
                *ty = WasmCompositeType::Struct(WasmStructType {
                    fields: vec![].into_boxed_slice(),
                });
                Ok(())
            })?;
        }

        match ty {
            WasmCompositeType::Array(ty) => self.mutate(c, ty),
            WasmCompositeType::Struct(ty) => self.mutate(c, ty),
            WasmCompositeType::Func(_) => unreachable!(),
        }
    }
}

impl Mutate<WasmArrayType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmArrayType,
    ) -> mutatis::Result<()> {
        self.mutate(c, &mut ty.0)
    }
}

impl Mutate<WasmStructType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmStructType,
    ) -> mutatis::Result<()> {
        let mut sub_mutator = m::boxed_slice(self);
        <m::BoxedSlice<_> as Mutate<Box<[WasmFieldType]>>>::mutate(
            &mut sub_mutator,
            c,
            &mut ty.fields,
        )
    }
}

impl Mutate<WasmFieldType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmFieldType,
    ) -> mutatis::Result<()> {
        // Mutability.
        c.mutation(|_| {
            ty.mutable = !ty.mutable;
            Ok(())
        })?;

        self.mutate(c, &mut ty.element_type)
    }
}

impl Mutate<WasmStorageType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmStorageType,
    ) -> mutatis::Result<()> {
        // Change to `i8`.
        if !ty.is_i8() {
            c.mutation(|_| {
                *ty = WasmStorageType::I8;
                Ok(())
            })?;
        }

        // Change to `i16`.
        if !ty.is_i16() {
            c.mutation(|_| {
                *ty = WasmStorageType::I16;
                Ok(())
            })?;
        }

        // Change to `ValType`.
        if !ty.is_val() {
            c.mutation(|_| {
                *ty = WasmStorageType::Val(WasmValType::Ref(WasmRefType {
                    nullable: true,
                    heap_type: WasmHeapType::Any,
                }));
                Ok(())
            })?;
        }

        // Mutate `ValType`.
        if let WasmStorageType::Val(ty) = ty {
            self.mutate(c, ty)?;
        }

        Ok(())
    }
}

impl Mutate<WasmValType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmValType,
    ) -> mutatis::Result<()> {
        if !matches!(ty, WasmValType::I32) {
            c.mutation(|_| {
                *ty = WasmValType::I32;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmValType::I64) {
            c.mutation(|_| {
                *ty = WasmValType::I64;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmValType::F32) {
            c.mutation(|_| {
                *ty = WasmValType::F32;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmValType::F64) {
            c.mutation(|_| {
                *ty = WasmValType::F64;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmValType::V128) {
            c.mutation(|_| {
                *ty = WasmValType::V128;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmValType::Ref(_)) {
            c.mutation(|_| {
                *ty = WasmValType::Ref(WasmRefType {
                    nullable: true,
                    heap_type: WasmHeapType::Any,
                });
                Ok(())
            })?;
        }
        if let WasmValType::Ref(ty) = ty {
            self.mutate(c, ty)?;
        }
        Ok(())
    }
}

impl Mutate<WasmRefType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmRefType,
    ) -> mutatis::Result<()> {
        c.mutation(|_| {
            ty.nullable = !ty.nullable;
            Ok(())
        })?;
        self.mutate(c, &mut ty.heap_type)
    }
}

impl Mutate<WasmHeapType> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut WasmHeapType,
    ) -> mutatis::Result<()> {
        if !matches!(ty, WasmHeapType::Extern) {
            c.mutation(|_| {
                *ty = WasmHeapType::Extern;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::NoExtern) {
            c.mutation(|_| {
                *ty = WasmHeapType::NoExtern;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::Func) {
            c.mutation(|_| {
                *ty = WasmHeapType::Func;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::ConcreteFunc(_)) {
            c.mutation(|_| {
                *ty = WasmHeapType::ConcreteFunc(EngineOrModuleTypeIndex::Module(
                    ModuleInternedTypeIndex::from_bits(0),
                ));
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::NoFunc) {
            c.mutation(|_| {
                *ty = WasmHeapType::NoFunc;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::Any) {
            c.mutation(|_| {
                *ty = WasmHeapType::Any;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::Eq) {
            c.mutation(|_| {
                *ty = WasmHeapType::Eq;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::I31) {
            c.mutation(|_| {
                *ty = WasmHeapType::I31;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::Array) {
            c.mutation(|_| {
                *ty = WasmHeapType::Array;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::ConcreteArray(_)) {
            c.mutation(|_| {
                *ty = WasmHeapType::ConcreteArray(EngineOrModuleTypeIndex::Module(
                    ModuleInternedTypeIndex::from_bits(0),
                ));
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::Struct) {
            c.mutation(|_| {
                *ty = WasmHeapType::Struct;
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::ConcreteStruct(_)) {
            c.mutation(|_| {
                *ty = WasmHeapType::ConcreteStruct(EngineOrModuleTypeIndex::Module(
                    ModuleInternedTypeIndex::from_bits(0),
                ));
                Ok(())
            })?;
        }
        if !matches!(ty, WasmHeapType::None) {
            c.mutation(|_| {
                *ty = WasmHeapType::None;
                Ok(())
            })?;
        }
        match ty {
            WasmHeapType::ConcreteFunc(ty)
            | WasmHeapType::ConcreteArray(ty)
            | WasmHeapType::ConcreteStruct(ty) => self.mutate(c, ty)?,
            _ => {}
        }
        Ok(())
    }
}

impl Mutate<EngineOrModuleTypeIndex> for TypesMutator {
    fn mutate(
        &mut self,
        c: &mut mutatis::Candidates<'_>,
        ty: &mut EngineOrModuleTypeIndex,
    ) -> mutatis::Result<()> {
        let mut idx = ty.unwrap_module_type_index().as_bits();
        let res = m::u32().mutate(c, &mut idx);
        *ty = EngineOrModuleTypeIndex::Module(ModuleInternedTypeIndex::from_bits(idx));
        res
    }
}
