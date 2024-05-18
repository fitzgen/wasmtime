//! Interpretation of pulley bytecode.

#![allow(warnings)] // TODO FITZGEN

use crate::decode::*;
use crate::regs::*;
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
            let mut continuation = self.decoder.unchecked_decode_one(&mut pc, &mut self.state);
            if let Continuation::ExtendedOp = &continuation {
                continuation = self
                    .decoder
                    .unchecked_decode_one_extended(&mut pc, &mut self.state);
            }
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
#[derive(Clone, Copy)]
pub enum Type {
    /// TODO FITZGEN
    XReg,
    /// TODO FITZGEN
    FReg,
    /// TODO FITZGEN
    VReg,
}

/// TODO FITZGEN
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

#[derive(Copy, Clone)]
union VRegUnion {
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
            todo!()
        }
    }
    fn jump(&mut self, offset: i32) -> Self::Return {
        todo!()
    }
    fn br_if(&mut self, cond: XReg, offset: i32) -> Self::Return {
        todo!()
    }
    fn br_if_not(&mut self, cond: XReg, offset: i32) -> Self::Return {
        todo!()
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

    fn load32_u(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        todo!()
    }
    fn load32_s(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        todo!()
    }
    fn load64(&mut self, dst: XReg, ptr: XReg) -> Self::Return {
        todo!()
    }

    fn load32_u_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        todo!()
    }
    fn load32_s_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        todo!()
    }
    fn load64_offset8(&mut self, dst: XReg, ptr: XReg, offset: i8) -> Self::Return {
        todo!()
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
        todo!()
    }
    fn store64(&mut self, ptr: XReg, src: XReg) -> Self::Return {
        todo!()
    }

    fn store32_offset8(&mut self, ptr: XReg, offset: i8, src: XReg) -> Self::Return {
        todo!()
    }
    fn store64_offset8(&mut self, ptr: XReg, offset: i8, src: XReg) -> Self::Return {
        todo!()
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
            .set_u32(u32::from_ne_bytes(val.to_ne_bytes()));
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
}
