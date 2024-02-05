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
//! rbx: qnan
//!
//! General purpose registers available for use by the VM:
//!
//! rax, rcx, rdx, r8-11
//!
//! Note that rcx, rdx, r8, r9 are used for argument passing when calling host functions.
//!
//! Predefined global variables (needed for unwinding/exiting):
//! [0] = emitter
//! [1] = RSP at the time of the call to the entry point

use crate::{
    assembler::{FloatOperand, FloatRegister, Label, Operand, Register},
    builtins::{
        concat_strings, handle_expected_add_operands, handle_expected_number, handle_global_uninit,
        print,
    },
    gc::GcCell,
    object::ObjString,
    source_mapping::{FnSourceInfo, SourceMapping},
    value::{Value, FALSE_VAL, NIL_VAL, QNAN, TRUE_VAL, UNINIT_VAL},
};
use memmap2::Mmap;

use crate::assembler::{Assembler, AssemblyOffset};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct GlobalVarIndex(usize);

pub struct FnInfo();

pub struct EntryPoint(AssemblyOffset);

fn exit_failure(assembler: &mut Assembler) {
    assembler.mov(
        Operand::Register(Register::Rsp),
        Operand::Memory(Register::R12, 8),
    );

    // Restore nonvolatile registers
    assembler.pop(Operand::Register(Register::R15));
    assembler.pop(Operand::Register(Register::R14));
    assembler.pop(Operand::Register(Register::R13));
    assembler.pop(Operand::Register(Register::R12));
    assembler.pop(Operand::Register(Register::Rsi));
    assembler.pop(Operand::Register(Register::Rdi));
    assembler.pop(Operand::Register(Register::Rbp));
    assembler.pop(Operand::Register(Register::Rbx));

    // return false
    assembler.mov(Operand::Register(Register::Rax), Operand::Imm64(0));
    assembler.ret();
}

fn create_error_handler(assembler: &mut Assembler, handler: u64) -> Label {
    let mut label = assembler.create_label();
    assembler.bind_label(&mut label);

    assembler.mov(
        Operand::Register(Register::Rcx),
        Operand::Memory(Register::Rsp, 0),
    );
    assembler.mov(
        Operand::Register(Register::Rdx),
        Operand::Register(Register::Rbp),
    );
    assembler.mov(
        Operand::Register(Register::R8),
        Operand::Memory(Register::R12, 0),
    );

    assembler.mov(Operand::Register(Register::Rax), Operand::Imm64(handler));
    assembler.call_extern(Operand::Register(Register::Rax), Register::R10);
    exit_failure(assembler);
    label
}

const REG_TRUE: Register = Register::R13;
const REG_FALSE: Register = Register::R14;
const REG_NIL: Register = Register::R15;
const REG_QNAN: Register = Register::Rbx;

pub struct Emitter {
    assembler: Assembler,
    executable: Option<Mmap>,
    globals: Vec<Value>,
    globals_names: Vec<GcCell<ObjString>>,
    source_mapping: SourceMapping,

    // Failure labels
    global_uninit: Label,
    expected_addition_operands: Label,
    expected_number: Label,
}

impl Emitter {
    pub fn new() -> Self {
        let mut assembler = Assembler::new();

        // Error handling functions
        let global_uninit = create_error_handler(&mut assembler, handle_global_uninit as u64);
        let expected_addition_operands =
            create_error_handler(&mut assembler, handle_expected_add_operands as u64);
        let expected_number = create_error_handler(&mut assembler, handle_expected_number as u64);

        let mut source_mapping = SourceMapping::new();
        source_mapping.begin_function(assembler.get_current_offset(), FnSourceInfo::new(None, 0));

        Self {
            assembler,
            executable: None,
            // Allocate space for the predefined global variables
            globals: vec![UNINIT_VAL; 2],
            source_mapping,
            globals_names: Vec::new(),
            global_uninit,
            expected_addition_operands,
            expected_number,
        }
    }

    pub fn create_entrypoint(&mut self) -> EntryPoint {
        let entry_point = EntryPoint(self.assembler.get_current_offset());
        // Preserve nonvolatile registers RBX, RBP, RDI, RSI, R12, R13, R14, R15
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

        // Store the pointer to the emitter as the first global variable
        self.assembler.mov(
            Operand::Memory(Register::R12, 0),
            Operand::Register(Register::Rdx),
        );
        // Store the RSP at the time of the call to the entry point as the second global variable
        self.assembler.mov(
            Operand::Memory(Register::R12, 8),
            Operand::Register(Register::Rsp),
        );

        self.assembler
            .mov(Operand::Register(REG_QNAN), Operand::Imm64(QNAN));
        self.assembler.mov(
            Operand::Register(REG_TRUE),
            Operand::Imm64(TRUE_VAL.to_bits()),
        );
        self.assembler.mov(
            Operand::Register(REG_FALSE),
            Operand::Imm64(FALSE_VAL.to_bits()),
        );
        self.assembler.mov(
            Operand::Register(REG_NIL),
            Operand::Imm64(NIL_VAL.to_bits()),
        );

        self.assembler.mov(
            Operand::Register(Register::Rbp),
            Operand::Register(Register::Rsp),
        );

        entry_point
    }

    pub fn get_globals_ptr(&mut self) -> *mut Value {
        self.globals.as_mut_ptr()
    }

    pub fn finish(
        &mut self,
        entry_point: EntryPoint,
    ) -> extern "win64" fn(*mut Value, *const Emitter) -> u8 {
        // Restore nonvolatile registers
        self.assembler.pop(Operand::Register(Register::R15));
        self.assembler.pop(Operand::Register(Register::R14));
        self.assembler.pop(Operand::Register(Register::R13));
        self.assembler.pop(Operand::Register(Register::R12));
        self.assembler.pop(Operand::Register(Register::Rsi));
        self.assembler.pop(Operand::Register(Register::Rdi));
        self.assembler.pop(Operand::Register(Register::Rbp));
        self.assembler.pop(Operand::Register(Register::Rbx));

        // return true
        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Imm64(1));
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

    pub fn print_stacktrace(&self, rip: *const u8, rbp: *const u8) {
        self.source_mapping
            .print_stacktrace(self.executable.as_ref().unwrap().as_ptr(), rip, rbp);
    }

    pub fn set_line(&mut self, line: usize) {
        self.source_mapping
            .set_line(self.assembler.get_current_offset(), line);
    }

    pub fn nil(&mut self) {
        self.assembler.push(Operand::Register(REG_NIL));
    }

    pub fn true_(&mut self) {
        self.assembler.push(Operand::Register(REG_TRUE));
    }

    pub fn false_(&mut self) {
        self.assembler.push(Operand::Register(REG_FALSE));
    }

    pub fn add_global(&mut self, name: GcCell<ObjString>) -> GlobalVarIndex {
        let index = GlobalVarIndex(self.globals.len());
        self.globals.push(UNINIT_VAL);
        self.globals_names.push(name);
        index
    }

    pub fn get_global_name(&self, index: GlobalVarIndex) -> GcCell<ObjString> {
        // Subtract 2 to account for the predefined global variables.
        self.globals_names[index.0 - 2]
    }

    pub fn define_global(&mut self, index: GlobalVarIndex) {
        self.assembler
            .pop(Operand::Memory(Register::R12, index.0 as i32 * 8));
    }

    fn check_global_initialized(&mut self, index: GlobalVarIndex) {
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Imm64(UNINIT_VAL.to_bits()),
        );
        self.assembler.cmp(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::R12, index.0 as i32 * 8),
        );
        let mut label_end = self.assembler.create_label();
        self.assembler.jne(&mut label_end);
        self.assembler.mov(
            Operand::Register(Register::R9),
            Operand::Imm64(index.0 as u64),
        );
        self.assembler.call_label(&mut self.global_uninit);
        self.assembler.bind_label(&mut label_end);
    }

    pub fn set_global(&mut self, index: GlobalVarIndex) {
        self.check_global_initialized(index);
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.assembler.mov(
            Operand::Memory(Register::R12, index.0 as i32 * 8),
            Operand::Register(Register::Rax),
        );
    }

    pub fn get_global(&mut self, index: GlobalVarIndex) {
        self.check_global_initialized(index);
        self.assembler
            .push(Operand::Memory(Register::R12, index.0 as i32 * 8));
    }

    pub fn set_local(&mut self, index: usize) {
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.assembler.mov(
            Operand::Memory(Register::Rbp, -(index as i32 * 8)),
            Operand::Register(Register::Rax),
        );
    }

    pub fn get_local(&mut self, index: usize) {
        self.assembler
            .push(Operand::Memory(Register::Rbp, -(index as i32 * 8)));
    }

    pub fn print(&mut self) {
        self.assembler.pop(Operand::Register(Register::Rcx));
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Imm64(print as u64),
        );
        self.assembler
            .call_extern(Operand::Register(Register::Rax), Register::R10);
    }

    pub fn push(&mut self, value: u64) {
        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Imm64(value));
        self.assembler.push(Operand::Register(Register::Rax));
    }

    pub fn pop(&mut self) {
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
    }

    pub fn not(&mut self) {
        let mut end = self.assembler.create_label();

        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );

        // Check for false
        self.assembler.cmp(
            Operand::Register(Register::Rax),
            Operand::Register(REG_FALSE),
        );
        self.assembler.cmove(
            Operand::Register(Register::Rax),
            Operand::Register(REG_TRUE),
        );
        self.assembler.je(&mut end);

        // Check for nil
        self.assembler
            .cmp(Operand::Register(Register::Rax), Operand::Register(REG_NIL));
        self.assembler.cmove(
            Operand::Register(Register::Rax),
            Operand::Register(REG_TRUE),
        );
        self.assembler.je(&mut end);

        // Values other than nil and false are truthy
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Register(REG_FALSE),
        );
        self.assembler.bind_label(&mut end);

        self.assembler.mov(
            Operand::Memory(Register::Rsp, 0),
            Operand::Register(Register::Rax),
        );
    }

    pub fn negate(&mut self) {
        let mut ok = self.assembler.create_label();

        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.is_non_numeric(Operand::Register(Register::Rax));
        self.assembler.jne(&mut ok);

        // The operand was an object.
        self.assembler.call_label(&mut self.expected_number);

        self.assembler.bind_label(&mut ok);
        self.assembler.mov(
            Operand::Register(Register::Rax),
            // The zero flag is the most significant bit of a floating point number.
            Operand::Imm64(0x8000_0000_0000_0000),
        );
        self.assembler.xor(
            Operand::Memory(Register::Rsp, 0),
            Operand::Register(Register::Rax),
        );
    }

    pub fn add(&mut self) {
        let mut add_strings = self.assembler.create_label();
        let mut end = self.assembler.create_label();

        // Load the operands into registers.
        self.assembler.mov(
            Operand::Register(Register::R9),
            Operand::Memory(Register::Rsp, 0),
        );
        self.assembler.mov(
            Operand::Register(Register::R8),
            Operand::Memory(Register::Rsp, 8),
        );

        // Is the second operand an object?
        self.assembler.movq_float_dest(
            FloatOperand::Register(FloatRegister::Xmm1),
            Operand::Register(Register::R9),
        );
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Register(Register::R9),
        );
        self.is_non_numeric(Operand::Register(Register::Rax));
        self.assembler.je(&mut add_strings);

        // is the first operand an object?
        self.assembler.movq_float_dest(
            FloatOperand::Register(FloatRegister::Xmm0),
            Operand::Register(Register::R8),
        );
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Register(Register::R8),
        );
        self.is_non_numeric(Operand::Register(Register::Rax));
        self.assembler.je(&mut add_strings);

        // Numeric addition.
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
        self.assembler.addsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Register(FloatRegister::Xmm1),
        );
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
        self.assembler.jmp(&mut end);

        // Attempt string concatenation.
        self.assembler.bind_label(&mut add_strings);
        self.call_gc(concat_strings as u64);
        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Imm8(16));
        self.assembler.push(Operand::Register(Register::Rax));

        // Check the result for errors.
        self.assembler.test(
            Operand::Register(Register::Rax),
            Operand::Register(Register::Rax),
        );
        self.assembler.jne(&mut end);

        // At least one argument was not a string.
        self.assembler
            .call_label(&mut self.expected_addition_operands);

        self.assembler.bind_label(&mut end);
    }

    /// If [val] is not a numeric, sets the zero/equals flag.
    /// The value is modified in the process.
    fn is_non_numeric(&mut self, val: Operand) {
        self.assembler.and(val, Operand::Register(REG_QNAN));
        self.assembler.cmp(val, Operand::Register(REG_QNAN));
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
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
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
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
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
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
        self.assembler.movsd(
            FloatOperand::Memory(Register::Rsp, 0),
            FloatOperand::Register(FloatRegister::Xmm0),
        );
    }

    pub fn equality(&mut self, eq_val: Register, ne_val: Register) {
        let mut non_numeric_cmp = self.assembler.create_label();
        let mut end = self.assembler.create_label();

        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.is_non_numeric(Operand::Register(Register::Rax));
        self.assembler.je(&mut non_numeric_cmp);

        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 8),
        );
        self.is_non_numeric(Operand::Register(Register::Rax));
        self.assembler.je(&mut non_numeric_cmp);

        // Numeric (floating point) comparison
        self.assembler.movsd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 0),
        );
        self.assembler.ucomisd(
            FloatOperand::Register(FloatRegister::Xmm0),
            FloatOperand::Memory(Register::Rsp, 8),
        );
        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Register(ne_val));
        self.assembler
            .cmove(Operand::Register(Register::Rax), Operand::Register(eq_val));
        self.assembler
            .cmovp(Operand::Register(Register::Rax), Operand::Register(ne_val));
        self.assembler.jmp(&mut end);

        // Non-numeric comparison
        self.assembler.bind_label(&mut non_numeric_cmp);
        self.assembler.mov(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 0),
        );
        self.assembler.cmp(
            Operand::Register(Register::Rax),
            Operand::Memory(Register::Rsp, 8),
        );
        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Register(ne_val));
        self.assembler
            .cmove(Operand::Register(Register::Rax), Operand::Register(eq_val));

        // Move the result to the stack
        self.assembler.bind_label(&mut end);

        self.assembler
            .add(Operand::Register(Register::Rsp), Operand::Imm8(8));
        self.assembler.mov(
            Operand::Memory(Register::Rsp, 0),
            Operand::Register(Register::Rax),
        );
    }

    pub fn eq(&mut self) {
        self.equality(REG_TRUE, REG_FALSE);
    }

    pub fn ne(&mut self) {
        self.equality(REG_FALSE, REG_TRUE);
    }

    /// Calls an external function with the beginning and end of the stack as first and second arguments.
    /// Therefore, additional arguments need to be passed in r8/r8 or on the stack.    
    fn call_gc(&mut self, address: u64) {
        self.assembler.mov(
            Operand::Register(Register::Rcx),
            Operand::Register(Register::Rsp),
        );
        self.assembler.mov(
            Operand::Register(Register::Rdx),
            Operand::Memory(Register::R12, 8),
        );
        self.assembler
            .mov(Operand::Register(Register::Rax), Operand::Imm64(address));
        self.assembler
            .call_extern(Operand::Register(Register::Rax), Register::R10);
    }
}
