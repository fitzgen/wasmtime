//! Interpretation of pulley bytecode.
//!
//! TODO FITZGEN: put this somewhere else, maybe crate top level.
//!
//! Principles:
//!
//! * Avoid materializing ops, just dispatch directly to the visitor instead.
//!
//! * Macro ops for common sequences, especially those we see from Wasmtime and
//!   Cranelift.
//!
//! * Very simple and fast to decode ops, avoid bitpacking unless it really pays
//!   for itself by lowering cache pressure.
//!
//! * Variable-sized operations to keep the most common ops small and allow for
//!   unbounded numbers of macro ops in the future.

#![allow(warnings)] // TODO FITZGEN

use crate::decode::*;
use crate::imms::*;
use crate::regs::*;
use crate::MaterializeOpsVisitor;
use alloc::string::ToString;
use alloc::{vec, vec::Vec};
use core::mem;
use core::ptr::{self, NonNull};

const DEFAULT_STACK_SIZE: usize = 1 << 20; // 1 MiB

/// TODO FITZGEN
pub struct Vm {
    decoder: Decoder,
    state: MachineState,
}

impl Default for Vm {
    fn default() -> Self {
        Vm::new()
    }
}

impl Vm {
    /// TODO FITZGEN
    pub fn new() -> Self {
        Self::with_stack(vec![0; DEFAULT_STACK_SIZE])
    }

    /// TODO FITZGEN
    pub fn with_stack(stack: Vec<u8>) -> Self {
        Self {
            decoder: Decoder::new(),
            state: MachineState::with_stack(stack),
        }
    }

    /// TODO FITZGEN
    pub fn state(&self) -> &MachineState {
        &self.state
    }

    /// TODO FITZGEN
    pub fn state_mut(&mut self) -> &mut MachineState {
        &mut self.state
    }

    /// TODO FITZGEN
    pub fn into_stack(self) -> Vec<u8> {
        self.state.stack
    }

    /// TODO FITZGEN
    ///
    /// Returns either the resulting values, or the PC at which a trap was
    /// raised.
    pub unsafe fn call<'a, 'b>(
        &'a mut self,
        func: NonNull<u8>,
        args: &[Val],
        rets: impl IntoIterator<Item = Type> + 'a,
    ) -> Result<impl Iterator<Item = Val> + 'a, *mut u8> {
        // NB: make sure this method stays in sync with
        // `Pbc64MachineDeps::compute_arg_locs`!

        let mut x_args = (0..16).map(|x| XReg::unchecked_new(x));
        let mut f_args = (0..16).map(|f| FReg::unchecked_new(f));
        let mut v_args = (0..16).map(|v| VReg::unchecked_new(v));

        for arg in args {
            match arg {
                Val::XReg(val) => match x_args.next() {
                    Some(reg) => self.state.set_x(reg, *val),
                    None => todo!("stack slots"),
                },
                Val::FReg(val) => match f_args.next() {
                    Some(reg) => self.state.set_f(reg, *val),
                    None => todo!("stack slots"),
                },
                Val::VReg(val) => match v_args.next() {
                    Some(reg) => self.state.set_v(reg, *val),
                    None => todo!("stack slots"),
                },
            }
        }

        self.state.pc = func.as_ptr();
        self.decoder.set_position(self.state.pc as usize);
        self.run()?;

        let mut x_rets = (0..16).map(|x| XReg::unchecked_new(x));
        let mut f_rets = (0..16).map(|f| FReg::unchecked_new(f));
        let mut v_rets = (0..16).map(|v| VReg::unchecked_new(v));

        Ok(rets.into_iter().map(move |ty| match ty {
            Type::XReg => match x_rets.next() {
                Some(reg) => Val::XReg(self.state.get_x(reg)),
                None => todo!("stack slots"),
            },
            Type::FReg => match f_rets.next() {
                Some(reg) => Val::FReg(self.state.get_f(reg)),
                None => todo!("stack slots"),
            },
            Type::VReg => match v_rets.next() {
                Some(reg) => Val::VReg(self.state.get_v(reg)),
                None => todo!("stack slots"),
            },
        }))
    }

    unsafe fn run(&mut self) -> Result<(), *mut u8> {
        loop {
            let mut pc = self.state.pc;

            // TODO FITZGEN: should `cfg` this materialization and logging on something...
            let mut materializer = MaterializeOpsVisitor;
            let mut visitor = SequencedVisitor::new(
                move |op, cont| {
                    if let Some(op) = op {
                        log::trace!("executed {pc:#p}: {op:?}");

                        // eprintln!("FITZGEN: <enter> to step");
                        // let mut buf = std::string::String::new();
                        // std::io::stdin().read_line(&mut buf).unwrap();
                    }
                    cont
                },
                &mut materializer,
                &mut self.state,
            );
            let mut continuation = self.decoder.unchecked_decode_one(&mut pc, &mut visitor);

            if let Continuation::ExtendedOp = &continuation {
                continuation = self
                    .decoder
                    .unchecked_decode_one_extended(&mut pc, &mut visitor);
            }
            log::trace!("FITZGEN: machine state = {:#?}", self.state);

            // Really wish we had `feature(explicit_tail_calls)`...
            match continuation {
                Continuation::Continue => {
                    self.state.pc = pc;
                    continue;
                }
                Continuation::Jumped => {
                    continue;
                }

                // Out-of-line slow paths marked `cold` and `inline(never)` to
                // improve codegen.
                Continuation::Trap => self.trap()?,
                Continuation::ReturnToHost => return self.return_to_host(),
                Continuation::HostCall => self.host_call()?,
                Continuation::ExtendedOp => self.unreachable_extended_op()?,
            }
        }
    }

    #[cold]
    #[inline(never)]
    fn return_to_host(&self) -> Result<(), *mut u8> {
        Ok(())
    }

    #[cold]
    #[inline(never)]
    fn trap(&self) -> Result<(), *mut u8> {
        Err(self.state.pc)
    }

    #[cold]
    #[inline(never)]
    fn host_call(&self) -> Result<(), *mut u8> {
        todo!()
    }

    #[cold]
    #[inline(never)]
    fn unreachable_extended_op(&self) -> Result<(), *mut u8> {
        unreachable!("already processed extended op")
    }
}

/// TODO FITZGEN
#[derive(Clone, Copy, Debug)]
pub enum Type {
    /// TODO FITZGEN
    XReg,
    /// TODO FITZGEN
    FReg,
    /// TODO FITZGEN
    VReg,
}

/// TODO FITZGEN
#[derive(Clone, Copy, Debug)]
pub enum Val {
    /// TODO FITZGEN
    XReg(XRegVal),
    /// TODO FITZGEN
    FReg(FRegVal),
    /// TODO FITZGEN
    VReg(VRegVal),
}

/// TODO FITZGEN
#[derive(Copy, Clone)]
pub struct XRegVal(XRegUnion);

impl core::fmt::Debug for XRegVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("XRegVal")
            // .field("as_i32", &self.get_i32())
            // .field("as_u32", &self.get_u32())
            // .field("as_i64", &self.get_i64())
            .field("as_u64", &self.get_u64())
            // .field("as_isize", &self.get_isize())
            // .field("as_usize", &self.get_usize())
            .finish()
    }
}

// NB: we always store these in little endian, so we have to `from_le_bytes`
// whenever we read and `to_le_bytes` whenever we store.
#[derive(Copy, Clone)]
union XRegUnion {
    i32: i32,
    u32: u32,
    i64: i64,
    u64: u64,
    isize: isize,
    usize: usize,
}

impl Default for XRegVal {
    fn default() -> Self {
        Self(unsafe { mem::zeroed() })
    }
}

impl XRegVal {
    /// TODO FITZGEN
    pub fn new_i32(x: i32) -> Self {
        let mut val = XRegVal::default();
        val.set_i32(x);
        val
    }

    /// TODO FITZGEN
    pub fn new_u32(x: u32) -> Self {
        let mut val = XRegVal::default();
        val.set_u32(x);
        val
    }

    /// TODO FITZGEN
    pub fn new_i64(x: i64) -> Self {
        let mut val = XRegVal::default();
        val.set_i64(x);
        val
    }

    /// TODO FITZGEN
    pub fn new_u64(x: u64) -> Self {
        let mut val = XRegVal::default();
        val.set_u64(x);
        val
    }

    /// TODO FITZGEN
    pub fn new_isize(x: isize) -> Self {
        let mut val = XRegVal::default();
        val.set_isize(x);
        val
    }

    /// TODO FITZGEN
    pub fn new_usize(x: usize) -> Self {
        let mut val = XRegVal::default();
        val.set_usize(x);
        val
    }

    /// TODO FITZGEN
    pub fn get_i32(&self) -> i32 {
        let x = unsafe { self.0.i32 };
        i32::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_u32(&self) -> u32 {
        let x = unsafe { self.0.u32 };
        u32::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_i64(&self) -> i64 {
        let x = unsafe { self.0.i64 };
        i64::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_u64(&self) -> u64 {
        let x = unsafe { self.0.u64 };
        u64::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_isize(&self) -> isize {
        let x = unsafe { self.0.isize };
        isize::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_usize(&self) -> usize {
        let x = unsafe { self.0.usize };
        usize::from_le_bytes(x.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn set_i32(&mut self, x: i32) {
        let x = i32::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.i32 = x };
    }

    /// TODO FITZGEN
    pub fn set_u32(&mut self, x: u32) {
        let x = u32::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.u32 = x };
    }

    /// TODO FITZGEN
    pub fn set_i64(&mut self, x: i64) {
        let x = i64::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.i64 = x };
    }

    /// TODO FITZGEN
    pub fn set_u64(&mut self, x: u64) {
        let x = u64::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.u64 = x };
    }

    /// TODO FITZGEN
    pub fn set_isize(&mut self, x: isize) {
        let x = isize::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.isize = x };
    }

    /// TODO FITZGEN
    pub fn set_usize(&mut self, x: usize) {
        let x = usize::from_ne_bytes(x.to_le_bytes());
        unsafe { self.0.usize = x };
    }
}

/// TODO FITZGEN
#[derive(Copy, Clone)]
pub struct FRegVal(FRegUnion);

impl core::fmt::Debug for FRegVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FRegVal")
            .field("as_f32", &self.get_f32())
            .field("as_f64", &self.get_f64())
            .finish()
    }
}

// NB: we always store these in little endian, so we have to `from_le_bytes`
// whenever we read and `to_le_bytes` whenever we store.
#[derive(Copy, Clone)]
union FRegUnion {
    f32: u32,
    f64: u64,
}

impl Default for FRegVal {
    fn default() -> Self {
        Self(unsafe { mem::zeroed() })
    }
}

impl FRegVal {
    /// TODO FITZGEN
    pub fn new_f32(f: f32) -> Self {
        let mut val = Self::default();
        val.set_f32(f);
        val
    }

    /// TODO FITZGEN
    pub fn new_f64(f: f64) -> Self {
        let mut val = Self::default();
        val.set_f64(f);
        val
    }

    /// TODO FITZGEN
    pub fn get_f32(&self) -> f32 {
        let val = unsafe { self.0.f32 };
        f32::from_le_bytes(val.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn get_f64(&self) -> f64 {
        let val = unsafe { self.0.f64 };
        f64::from_le_bytes(val.to_ne_bytes())
    }

    /// TODO FITZGEN
    pub fn set_f32(&mut self, val: f32) {
        unsafe {
            self.0.f32 = u32::from_ne_bytes(val.to_le_bytes());
        }
    }

    /// TODO FITZGEN
    pub fn set_f64(&mut self, val: f64) {
        unsafe {
            self.0.f64 = u64::from_ne_bytes(val.to_le_bytes());
        }
    }
}

/// TODO FITZGEN
#[derive(Copy, Clone)]
pub struct VRegVal(VRegUnion);

impl core::fmt::Debug for VRegVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("VRegVal")
            .field("as_i128", &unsafe { self.0.i128 })
            .field("as_u128", &unsafe { self.0.u128 })
            .finish()
    }
}

#[derive(Copy, Clone)]
union VRegUnion {
    // TODO FITZGEN: need to figure out how we are going to handle portability
    // of lane ordering on top of each lane's endianness.
    i128: i128,
    u128: u128,
}

impl Default for VRegVal {
    fn default() -> Self {
        Self(unsafe { mem::zeroed() })
    }
}

/// TODO FITZGEN
pub struct MachineState {
    pc: *mut u8,
    x_regs: [XRegVal; XReg::RANGE.end as usize],
    f_regs: [FRegVal; FReg::RANGE.end as usize],
    v_regs: [VRegVal; VReg::RANGE.end as usize],
    stack: Vec<u8>,
}

unsafe impl Send for MachineState {}
unsafe impl Sync for MachineState {}

impl core::fmt::Debug for MachineState {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let MachineState {
            pc,
            x_regs,
            f_regs,
            v_regs,
            stack: _,
        } = self;

        struct RegMap<'a, R>(&'a [R], fn(u8) -> alloc::string::String);

        impl<R: core::fmt::Debug> core::fmt::Debug for RegMap<'_, R> {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let mut f = f.debug_map();
                for (i, r) in self.0.iter().enumerate() {
                    f.entry(&(self.1)(i as u8), r);
                }
                f.finish()
            }
        }

        f.debug_struct("MachineState")
            .field("pc", pc)
            .field(
                "x_regs",
                &RegMap(x_regs, |i| XReg::new(i).unwrap().to_string()),
            )
            // .field(
            //     "f_regs",
            //     &RegMap(f_regs, |i| FReg::new(i).unwrap().to_string()),
            // )
            // .field(
            //     "v_regs",
            //     &RegMap(v_regs, |i| VReg::new(i).unwrap().to_string()),
            // )
            .finish_non_exhaustive()
    }
}

impl MachineState {
    fn with_stack(stack: Vec<u8>) -> Self {
        assert!(stack.len() > 0);
        let mut state = Self {
            pc: ptr::null_mut(),
            x_regs: [Default::default(); XReg::RANGE.end as usize],
            f_regs: Default::default(),
            v_regs: Default::default(),
            stack,
        };

        let sp = state.stack.last().unwrap() as *const u8 as usize;
        state.set_x(XReg::SP, XRegVal::new_usize(sp));

        state.set_x(XReg::FP, XRegVal::new_i64(-1));
        state.set_x(XReg::LR, XRegVal::new_i64(-1));

        state
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn x(&self, x: XReg) -> &XRegVal {
        debug_assert!(x.index() < self.x_regs.len());
        unsafe { self.x_regs.get_unchecked(x.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn x_mut(&mut self, x: XReg) -> &mut XRegVal {
        debug_assert!(x.index() < self.x_regs.len());
        unsafe { self.x_regs.get_unchecked_mut(x.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn get_x(&self, x: XReg) -> XRegVal {
        *self.x(x)
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn set_x(&mut self, x: XReg, val: XRegVal) {
        debug_assert!(x.index() < self.x_regs.len());
        unsafe {
            *self.x_regs.get_unchecked_mut(x.index()) = val;
        }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn f(&self, f: FReg) -> &FRegVal {
        debug_assert!(f.index() < self.f_regs.len());
        unsafe { self.f_regs.get_unchecked(f.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn f_mut(&mut self, f: FReg) -> &mut FRegVal {
        debug_assert!(f.index() < self.f_regs.len());
        unsafe { self.f_regs.get_unchecked_mut(f.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn get_f(&self, f: FReg) -> FRegVal {
        debug_assert!(f.index() < self.f_regs.len());
        unsafe { *self.f_regs.get_unchecked(f.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn set_f(&mut self, f: FReg, val: FRegVal) {
        debug_assert!(f.index() < self.f_regs.len());
        unsafe {
            *self.f_regs.get_unchecked_mut(f.index()) = val;
        }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn v(&self, v: VReg) -> &VRegVal {
        debug_assert!(v.index() < self.v_regs.len());
        unsafe { self.v_regs.get_unchecked(v.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn v_mut(&mut self, v: VReg) -> &mut VRegVal {
        debug_assert!(v.index() < self.v_regs.len());
        unsafe { self.v_regs.get_unchecked_mut(v.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn get_v(&self, v: VReg) -> VRegVal {
        debug_assert!(v.index() < self.v_regs.len());
        unsafe { *self.v_regs.get_unchecked(v.index()) }
    }

    /// TODO FITZGEN
    #[inline(always)]
    pub fn set_v(&mut self, v: VReg, val: VRegVal) {
        debug_assert!(v.index() < self.v_regs.len());
        unsafe {
            *self.v_regs.get_unchecked_mut(v.index()) = val;
        }
    }

    #[inline(always)]
    fn pc_rel_jump(&mut self, offset: PcRelOffset) -> Continuation {
        let offset = isize::try_from(i32::from(offset)).unwrap();
        let new_pc = (self.pc as usize).wrapping_add(offset as usize);
        let new_pc = new_pc as *mut u8;
        log::trace!("jumping to {new_pc:#p}");
        self.pc = new_pc;
        Continuation::Jumped
    }
}

#[doc(hidden)]
pub enum Continuation {
    Continue,
    Jumped,
    ExtendedOp,
    ReturnToHost,
    Trap,

    // TODO: Should we return this or just do the host call inside the visitor
    // directly?
    HostCall,
}

#[doc(hidden)]
impl OpVisitor for MachineState {
    type Return = Continuation;

    fn ret(&mut self) -> Self::Return {
        if self.x(XReg::LR).get_u64() == u64::MAX {
            Continuation::ReturnToHost
        } else {
            let return_addr = self.x(XReg::LR).get_usize() as *mut u8;
            log::trace!("returning to {return_addr:#p}");
            self.pc = return_addr;
            Continuation::Jumped
        }
    }
    fn call(&mut self, offset: PcRelOffset) -> Self::Return {
        let return_addr = u64::try_from(self.pc as usize + 5).unwrap();
        self.x_mut(XReg::LR).set_u64(return_addr);
        self.pc_rel_jump(offset)
    }
    fn jump(&mut self, offset: PcRelOffset) -> Self::Return {
        self.pc_rel_jump(offset)
    }
    fn br_if(&mut self, cond: XReg, offset: PcRelOffset) -> Self::Return {
        let cond = self.x(cond).get_u64();
        if cond != 0 {
            self.pc_rel_jump(offset)
        } else {
            Continuation::Continue
        }
    }
    fn br_if_not(&mut self, cond: XReg, offset: PcRelOffset) -> Self::Return {
        let cond = self.x(cond).get_u64();
        if cond == 0 {
            self.pc_rel_jump(offset)
        } else {
            Continuation::Continue
        }
    }
    fn xmov(&mut self, dst: XReg, src: XReg) -> Self::Return {
        let val = self.get_x(src);
        self.set_x(dst, val);
        Continuation::Continue
    }
    fn fmov(&mut self, dst: FReg, src: FReg) -> Self::Return {
        let val = self.get_f(src);
        self.set_f(dst, val);
        Continuation::Continue
    }
    fn vmov(&mut self, dst: VReg, src: VReg) -> Self::Return {
        let val = self.get_v(src);
        self.set_v(dst, val);
        Continuation::Continue
    }
    fn xconst8(&mut self, dst: XReg, imm: u8) -> Self::Return {
        self.x_mut(dst).set_u64(u64::from(imm));
        Continuation::Continue
    }
    fn xconst16(&mut self, dst: XReg, imm: u16) -> Self::Return {
        self.x_mut(dst).set_u64(u64::from(imm));
        Continuation::Continue
    }
    fn xconst32(&mut self, dst: XReg, imm: u32) -> Self::Return {
        self.x_mut(dst).set_u64(u64::from(imm));
        Continuation::Continue
    }
    fn xconst64(&mut self, dst: XReg, imm: u64) -> Self::Return {
        self.x_mut(dst).set_u64(imm);
        Continuation::Continue
    }

    fn xadd32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u32();
        let b = self.x(src2).get_u32();
        self.x_mut(dst).set_u32(a.wrapping_add(b));
        Continuation::Continue
    }
    fn xadd64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u64();
        let b = self.x(src2).get_u64();
        self.x_mut(dst).set_u64(a.wrapping_add(b));
        Continuation::Continue
    }

    fn xeq64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u64();
        let b = self.x(src2).get_u64();
        self.x_mut(dst).set_u64(u64::from(a == b));
        Continuation::Continue
    }

    fn xneq64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u64();
        let b = self.x(src2).get_u64();
        self.x_mut(dst).set_u64(u64::from(a != b));
        Continuation::Continue
    }

    fn xslt64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_i64();
        let b = self.x(src2).get_i64();
        self.x_mut(dst).set_u64(u64::from(a < b));
        Continuation::Continue
    }

    fn xslteq64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_i64();
        let b = self.x(src2).get_i64();
        self.x_mut(dst).set_u64(u64::from(a <= b));
        Continuation::Continue
    }

    fn xult64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u64();
        let b = self.x(src2).get_u64();
        self.x_mut(dst).set_u64(u64::from(a < b));
        Continuation::Continue
    }

    fn xulteq64(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u64();
        let b = self.x(src2).get_u64();
        self.x_mut(dst).set_u64(u64::from(a <= b));
        Continuation::Continue
    }

    fn xeq32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u32();
        let b = self.x(src2).get_u32();
        self.x_mut(dst).set_u64(u64::from(a == b));
        Continuation::Continue
    }

    fn xneq32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u32();
        let b = self.x(src2).get_u32();
        self.x_mut(dst).set_u64(u64::from(a != b));
        Continuation::Continue
    }

    fn xslt32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_i32();
        let b = self.x(src2).get_i32();
        self.x_mut(dst).set_u64(u64::from(a < b));
        Continuation::Continue
    }

    fn xslteq32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_i32();
        let b = self.x(src2).get_i32();
        self.x_mut(dst).set_u64(u64::from(a <= b));
        Continuation::Continue
    }

    fn xult32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u32();
        let b = self.x(src2).get_u32();
        self.x_mut(dst).set_u64(u64::from(a < b));
        Continuation::Continue
    }

    fn xulteq32(&mut self, dst: XReg, src1: XReg, src2: XReg) -> Self::Return {
        let a = self.x(src1).get_u32();
        let b = self.x(src2).get_u32();
        self.x_mut(dst).set_u64(u64::from(a <= b));
        Continuation::Continue
    }

    fn load32_u(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let ptr = ptr as *mut u32;
        let val = unsafe { ptr::read(ptr) };
        self.x_mut(dst).set_u64(u64::from(val));
        Continuation::Continue
    }
    fn load32_s(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        todo!()
    }
    fn load64(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let ptr = ptr as *mut u64;
        let val = unsafe { ptr::read(ptr) };
        self.x_mut(dst).set_u64(val);
        Continuation::Continue
    }

    fn load32_u_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let offset = isize::from(offset);
        let ptr = ptr.wrapping_add(offset as usize);
        let ptr = ptr as *mut u32;
        let val = unsafe { ptr::read(ptr) };
        self.x_mut(dst).set_u64(u64::from(val));
        Continuation::Continue
    }
    fn load32_s_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        todo!()
    }
    fn load64_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let offset = isize::from(offset);
        let ptr = ptr.wrapping_add(offset as usize);
        let ptr = ptr as *mut u64;
        let val = unsafe { ptr::read(ptr) };
        self.x_mut(dst).set_u64(val);
        Continuation::Continue
    }

    fn load32_u_offset16(&mut self, dst: XReg, ptr: XReg, offset: i16) -> Self::Return {
        todo!()
    }
    fn load32_s_offset16(&mut self, dst: XReg, ptr: XReg, offset: i16) -> Self::Return {
        todo!()
    }
    fn load64_offset16(&mut self, dst: XReg, ptr: XReg, offset: i16) -> Self::Return {
        todo!()
    }

    fn load32_u_offset32(&mut self, dst: XReg, ptr: XReg, offset: i32) -> Self::Return {
        todo!()
    }
    fn load32_s_offset32(&mut self, dst: XReg, ptr: XReg, offset: i32) -> Self::Return {
        todo!()
    }
    fn load64_offset32(&mut self, dst: XReg, ptr: XReg, offset: i32) -> Self::Return {
        todo!()
    }

    fn load32_u_offset64(&mut self, dst: XReg, ptr: XReg, offset: i64) -> Self::Return {
        todo!()
    }
    fn load32_s_offset64(&mut self, dst: XReg, ptr: XReg, offset: i64) -> Self::Return {
        todo!()
    }
    fn load64_offset64(&mut self, dst: XReg, ptr: XReg, offset: i64) -> Self::Return {
        todo!()
    }

    fn store32(&mut self, ptr: XReg, src: XReg) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let ptr = ptr as *mut u32;
        let val = self.x(src).get_u32();
        unsafe {
            ptr::write(ptr, val);
        }
        Continuation::Continue
    }
    fn store64(&mut self, ptr: XReg, src: XReg) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let ptr = ptr as *mut u64;
        let val = self.x(src).get_u64();
        unsafe {
            ptr::write(ptr, val);
        }
        Continuation::Continue
    }

    fn store32_offset8(&mut self, ptr: XReg, offset: i8, src: XReg) -> Self::Return {
        todo!()
    }
    fn store64_offset8(&mut self, ptr: XReg, offset: i8, src: XReg) -> Self::Return {
        let ptr = self.x(ptr).get_usize();
        let offset = isize::from(offset);
        let ptr = ptr.wrapping_add(offset as usize);
        let ptr = ptr as *mut u64;
        let val = self.x(src).get_u64();
        unsafe {
            ptr::write(ptr, val);
        }
        Continuation::Continue
    }

    fn store32_offset16(&mut self, ptr: XReg, offset: i16, src: XReg) -> Self::Return {
        todo!()
    }
    fn store64_offset16(&mut self, ptr: XReg, offset: i16, src: XReg) -> Self::Return {
        todo!()
    }

    fn store32_offset32(&mut self, ptr: XReg, offset: i32, src: XReg) -> Self::Return {
        todo!()
    }
    fn store64_offset32(&mut self, ptr: XReg, offset: i32, src: XReg) -> Self::Return {
        todo!()
    }

    fn store32_offset64(&mut self, ptr: XReg, offset: i64, src: XReg) -> Self::Return {
        todo!()
    }
    fn store64_offset64(&mut self, ptr: XReg, offset: i64, src: XReg) -> Self::Return {
        todo!()
    }

    fn bitcast_int_from_float_32(&mut self, dst: XReg, src: FReg) -> Self::Return {
        let val = self.f(src).get_f32();
        self.x_mut(dst)
            .set_u64(u32::from_ne_bytes(val.to_ne_bytes()).into());
        Continuation::Continue
    }
    fn bitcast_int_from_float_64(&mut self, dst: XReg, src: FReg) -> Self::Return {
        let val = self.f(src).get_f64();
        self.x_mut(dst)
            .set_u64(u64::from_ne_bytes(val.to_ne_bytes()));
        Continuation::Continue
    }
    fn bitcast_float_from_int_32(&mut self, dst: FReg, src: XReg) -> Self::Return {
        let val = self.x(src).get_u32();
        self.f_mut(dst)
            .set_f32(f32::from_ne_bytes(val.to_ne_bytes()));
        Continuation::Continue
    }
    fn bitcast_float_from_int_64(&mut self, dst: FReg, src: XReg) -> Self::Return {
        let val = self.x(src).get_u64();
        self.f_mut(dst)
            .set_f64(f64::from_ne_bytes(val.to_ne_bytes()));
        Continuation::Continue
    }

    fn extended_op(&mut self) -> Self::Return {
        Continuation::ExtendedOp
    }
}

impl ExtendedOpVisitor for MachineState {
    fn nop(&mut self) -> Self::Return {
        Continuation::Continue
    }

    fn trap(&mut self) -> Self::Return {
        Continuation::Trap
    }

    fn get_sp(&mut self, dst: XReg) -> Self::Return {
        let sp = self.x(XReg::SP).get_u64();
        self.x_mut(dst).set_u64(sp);
        Continuation::Continue
    }
}
