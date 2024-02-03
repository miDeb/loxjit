//! Internal calling convention:
//!
//! [ this (implicit first argument) ] [ explicit function arguments ]* [ saved rsi ] [ saved rip ] [ saved rbp ] [ local variables ]*
//! ^
//! |
//!  base pointer
//!
//! rsi: (re-)stored by the caller
//! rip: (re-)stored by call/ret instruction
//! rbp: (re-)stored by the callee
//!
//! Registers used by the VM:
//!
//! rsi: pointer to the current ObjClosure (used to read/store upvalues)
//! rbp: base pointer of the current call frame
//! rdi: call frame depth counter for stack overflow checks
//! r12: ptr to globals table (=a heap allocated vector)
//! r13-15: true, false, nil
//!
//! General purpose registers available for use by the VM:
//!
//! rax, rbx, rcx, rdx, r8-11
//!
//! Note that rcx, rdx, r8, r9 are used for argument passing when calling host functions.

use crate::{
    assembler::{FloatOperand, FloatRegister, Operand, Register},
    builtins::print,
    value::{Value, UNINIT_VAL},
};
use memmap2::Mmap;

use crate::{
    assembler::{Assembler, AssemblyOffset},
    gc::GcCell,
    object::ObjString,
};

#[derive(Clone, Copy)]
pub struct GlobalVarIndex(usize);

pub struct FnInfo();

pub struct EntryPoint(AssemblyOffset);

pub struct Emitter {
    assembler: Assembler,
    executable: Option<Mmap>,
    globals: Vec<Value>,
}
impl Emitter {
    pub fn new() -> Self {
        let assembler = Assembler::new();
        Self {
            assembler,
            executable: None,
            globals: Vec::new(),
        }
    }

    pub fn create_entrypoint(&mut self) -> EntryPoint {
        let entry_point = EntryPoint(self.assembler.get_current_offset());
        // Preserve nonvolatile registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15
        self.assembler.push(Operand::Register(Register::Rbx));
        self.assembler.push(Operand::Register(Register::Rbp));
        self.assembler.push(Operand::Register(Register::Rdi));
        self.assembler.push(Operand::Register(Register::Rsi));
        self.assembler.push(Operand::Register(Register::R12));
        self.assembler.push(Operand::Register(Register::R13));
        self.assembler.push(Operand::Register(Register::R14));
        self.assembler.push(Operand::Register(Register::R15));

        // Store the globals table ptr in r12
        self.assembler.mov(
            Operand::Register(Register::R12),
            Operand::Register(Register::Rcx),
        );
        entry_point
    }

    pub fn get_globals_ptr(&mut self) -> *mut Value {
        self.globals.as_mut_ptr()
    }

    pub fn finish(&mut self, entry_point: EntryPoint) -> extern "win64" fn(*mut Value) -> u8 {
        // Restore nonvolatile registers
        self.assembler.pop(Operand::Register(Register::R15));
        self.assembler.pop(Operand::Register(Register::R14));
        self.assembler.pop(Operand::Register(Register::R13));
        self.assembler.pop(Operand::Register(Register::R12));
        self.assembler.pop(Operand::Register(Register::Rsi));
        self.assembler.pop(Operand::Register(Register::Rdi));
        self.assembler.pop(Operand::Register(Register::Rbp));
        self.assembler.pop(Operand::Register(Register::Rbx));

        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Immediate(1));
        self.assembler.ret();
        let mmap = self.assembler.make_executable();
        self.executable = Some(mmap);
        unsafe {
            std::mem::transmute(
                self.executable
                    .as_ref()
                    .unwrap()
                    .as_ptr()
                    .add(entry_point.0 .0),
            )
        }
    }

    pub fn add_global(&mut self) -> GlobalVarIndex {
        let index = GlobalVarIndex(self.globals.len());
        self.globals.push(UNINIT_VAL);
        index
    }

    pub fn define_global(&mut self, index: GlobalVarIndex) {
        self.assembler
            .pop(Operand::Memory(Register::R12, index.0 as i32));
    }

    fn check_global_initialized(&mut self, index: GlobalVarIndex) {
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Immediate(UNINIT_VAL.to_bits()),
        );
        self.assembler.cmp(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::R12, index.0 as i32),
        );
        // TODO: error handling (je)
    }

    pub fn set_global(&mut self, index: GlobalVarIndex) {
        self.check_global_initialized(index);
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.assembler.mov(
            Operand::Memory(Register::R12, index.0 as i32),
            Operand::Register(Register::Rax),
        );
    }

    pub fn get_global(&mut self, index: GlobalVarIndex) {
        self.assembler
            .push(Operand::Memory(Register::R12, index.0 as i32));
    }

    pub fn print(&mut self) {
        self.assembler.pop(Operand::Register(Register::Rcx));
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Immediate(print as u64),
        );
        self.assembler
            .call(Operand::Register(Register::Rax), Register::R10);
    }

    pub fn number(&mut self, value: f64) {
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Immediate(value.to_bits()),
        );
        self.assembler.push(Operand::Register(Register::Rax));
    }

    pub fn pop(&mut self) {
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Immediate(8));
    }

    pub fn add(&mut self) {
        self.assembler.movsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Immediate(8));
        self.assembler.addsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
    }

    pub fn sub(&mut self) {
        self.assembler.movsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 8),
        );
        self.assembler.subsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Immediate(8));
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
    }

    pub fn mul(&mut self) {
        self.assembler.movsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 8),
        );
        self.assembler.mulsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Immediate(8));
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
    }

    pub fn div(&mut self) {
        self.assembler.movsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 8),
        );
        self.assembler.divsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Immediate(8));
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
    }
}
