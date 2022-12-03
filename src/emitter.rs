//! Calling convention:
//!
//! [ saved rsi ] [ saved rip ] [ saved rbp ]
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

use std::mem::MaybeUninit;
use std::ops::Range;
use std::ptr::{null, null_mut};
use std::time::Instant;

use crate::common::{ENABLE_ICS, LOX_LOX_EXTENSIONS};
use crate::gc::{intern_string, register_const_object, register_object, GcCell};
use crate::object::{
    ObjBoundMethod, ObjClass, ObjClosure, ObjInstance, ObjString, ObjType, ObjUpvalue, INIT_STRING,
};
use crate::properties::{ObjShape, ShapeEntry};
use crate::source_mapping::{FnSourceInfo, SourceMapping};
use crate::value::{Value, FALSE_VAL, NIL_VAL, QNAN, SIGN_BIT, TRUE_VAL, UNINIT_VAL};
use crate::{INPUT_STREAM, START};
use dynasmrt::x64::Assembler;
use dynasmrt::{dynasm, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi};
use utf8_chars::BufReadCharsExt;

static mut GLOBALS: Vec<u64> = Vec::new();
static mut GLOBALS_NAMES: Vec<GcCell<ObjString>> = Vec::new();
const RESERVED_GLOBAL_VARS: usize = 9;

static mut ASSEMBLER: *mut Assembler = null_mut();
static mut ASSEMBLY_BASE: *const u8 = null();

const FRAMES_MAX: u32 = if LOX_LOX_EXTENSIONS { 0xff } else { 0x40 };

pub fn global_vars() -> &'static [Value] {
    unsafe { std::mem::transmute(&GLOBALS[RESERVED_GLOBAL_VARS..]) }
}

macro_rules! my_dynasm {
    ($ops:expr, $($t:tt)*) => {
        dynasm!($ops
            ; .arch x64
            ; .alias true_val, r13
            ; .alias false_val, r14
            ; .alias nil_val, r15
            $($t)*
        )
    }
}

/// Utility to make sure we use the right offsets to overwrite the ICs
fn check_asm_offsets(current: usize, starts: &[usize], expected: &[usize]) {
    if !ENABLE_ICS {
        return;
    }
    assert_eq!(starts.len(), expected.len());
    for (i, offset) in starts.iter().enumerate() {
        assert_eq!(expected[i], current - offset);
    }
}

macro_rules! call_extern {
    ($ops:expr, $fun:expr $(,$check_offsets:expr, $expected_offsets:expr)?) => {
        dynasm!($ops
            // We need to align the stack to 0x10.
            ; push rbp
            ; mov rbp, rsp
            ; and spl, BYTE 0xF0 as _

            ; mov rax, QWORD $fun as _
            ; call rax
            $(
                // When calling a function that will update the preceding IC we make sure
                // the function will use the right offset from the return address.
                ;; check_asm_offsets($ops.offset().0, &$check_offsets, &$expected_offsets)
            )?
            ; mov rsp, rbp
            ; pop rbp
        )
    };
}

macro_rules! eq {
    ($ops:expr, $equal_val:tt, $not_equal_val:tt) => {
         my_dynasm!($ops,
             ; mov rax, [rsp]
             ; and rax, [->qnan]
             ; cmp rax, [->qnan]
             ; je >non_numeric_cmp
             ; mov rax, [rsp + 8]
             ; and rax, [->qnan]
             ; cmp rax, [->qnan]
             ; je >non_numeric_cmp

             // numeric cmp:
             ; movq xmm0, [rsp]
             ; add rsp, 8
             ; movq xmm1, [rsp]
             ; ucomisd xmm0, xmm1
             ; cmove rax, $equal_val
             ; cmovne rax, $not_equal_val
             ; cmovp rax, $not_equal_val
             ; jmp > end

             // non-numeric cmp:
             ; non_numeric_cmp:
             ; mov rax, [rsp]
             ; add rsp, 8
             ; mov r8, [rsp]
             ; cmp rax, r8
             ; cmove rax, $equal_val
             ; cmovne rax, $not_equal_val

             ; end:
             ; mov [rsp], rax
         )

    };
}

macro_rules! handle_error {
    ($ops:expr, $fun:expr) => {
        dynasm!($ops
            ; mov rcx, [rsp]
            ; mov rdx, rbp
            ;; call_extern!($ops, $fun)
            ; jmp ->exit_error
        )
    };
}

/// Loads the current assembly offset into a variable, adding additional offset.
macro_rules! load_asm_offset {
    ($ops:expr, $target:ident, $additional_offset:literal) => {
        $target = $ops.offset().0 + $additional_offset
    };
}

const GET_PROP_IC_SHAPE_OFFSET: usize = 0x47;
const GET_PROP_IC_INDEX_OFFSET: usize = 0x2F;
macro_rules! get_property_ic {
    ($ops:expr, $stack_offset:expr) => {
        if ENABLE_ICS {
            let shape_offset;
            let index_offset;
            dynasm!($ops
                // rax = receiver.shape
                ; mov rax, QWORD [rcx + 0x10]
                ;; load_asm_offset!($ops, shape_offset, 0x2)
                ; mov r8, QWORD 0 as _
                ; cmp r8, rax
                ; jne >end_ic
                // rax = receiver.fields
                ; mov rax, QWORD [rcx + 0x18]
                ;; load_asm_offset!($ops, index_offset, 0x3)
                ; mov rax, QWORD [rax + i32::MAX]
                ; mov QWORD [rsp + $stack_offset], rax
                ; jmp >ok

                ; end_ic:
            );
            [shape_offset, index_offset]
        } else {
            [0, 0]
        }
    };
}

const SET_PROP_IC_SHAPE_OFFSET: usize = 0x61;
const SET_PROP_IC_INDEX_OFFSET: usize = 0x48;
const SET_PROP_IC_OFFSET: usize = 0x4B;
const SET_PROP_IC_END_JMP_OFFSET: usize = 0x3F;

const SET_PROP_IC_LEN: usize = 0x28;
macro_rules! set_property_ic {
    ($ops:expr) => {
        if ENABLE_ICS {
            let shape_offset;
            let index_offset;
            let ic_offset;
            let jmp_offset;
            dynasm!($ops
                // rax = receiver.shape
                ; mov rax, QWORD [rcx + 0x10]
                ;; load_asm_offset!($ops, shape_offset, 0x2)
                ; mov r8, QWORD 0 as _
                ; cmp r8, rax
                ; jne >end_ic
                // rax = receiver.fields
                ; mov rax, QWORD [rcx + 0x18]
                ; pop rdx
                ;; load_asm_offset!($ops, ic_offset, 0)
                ;; load_asm_offset!($ops, index_offset, 0x3)
                ; mov QWORD [rax + i32::MAX], rdx
                ; add rsp, 0x8
                ; push rdx
                ;; load_asm_offset!($ops, jmp_offset, 0)
                ; jmp >ok
                ;; for _ in 0..0x17 {
                    dynasm!($ops
                        ; nop
                    )
                }
                ;; assert_eq!(ic_offset + SET_PROP_IC_LEN, $ops.offset().0)
                ; jmp >ok
                ; end_ic:
            );
            [shape_offset, index_offset, ic_offset, jmp_offset]
        } else {
            [0, 0, 0, 0]
        }
    };
}

const INVOKE_IC_SHAPE_OFFSET: usize = 62;
const INVOKE_IC_METHOD_OFFSET: usize = 43;
macro_rules! invoke_ic {
    ($ops:expr) => {
        if ENABLE_ICS {
            let shape_offset;
            let method_offset;

            dynasm!($ops
                // rax = receiver.shape
                ; mov rax, QWORD [rcx + 0x10]
                ;; load_asm_offset!($ops, shape_offset, 0x2)
                ; mov r8, QWORD 0 as i64
                ; cmp r8, rax
                ; jne >end_ic
                ;; load_asm_offset!($ops, method_offset, 0x2)
                ; mov rax, QWORD 0 as i64
                ; jmp >after_resolve_method

                ; end_ic:
            );

            [shape_offset, method_offset]
        } else {
            [0, 0]
        }
    };
}

fn entrypoint_prologue(ops: &mut Assembler) {
    my_dynasm!(ops,
        ; mov [rcx], r15
        ; mov [rcx+0x8], r14
        ; mov [rcx+0x10], r13
        ; mov [rcx+0x18], r12
        ; mov [rcx+0x20], rsi
        ; mov [rcx+0x28], rdi
        ; mov [rcx+0x30], rbp
        ; mov [rcx+0x38], rbx
        ; mov [rcx+0x40], rsp
        ; mov r12, rcx
        ; mov true_val, QWORD TRUE_VAL.to_bits() as _
        ; mov false_val, QWORD FALSE_VAL.to_bits() as _
        ; mov nil_val, QWORD NIL_VAL.to_bits() as _
        ; mov rbp, rsp
        ; xor edi, edi
    );
}

enum BinaryOp {
    Sub,
    Mul,
    Div,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone, Copy)]
pub struct GlobalVarIndex(i32);

#[derive(Clone, Copy)]
pub struct FnInfo {
    start: DynamicLabel,
    epilogue: DynamicLabel,
    arity: u8,
}

pub struct Emitter {
    ops: Assembler,
    start: AssemblyOffset,
}

impl Emitter {
    pub fn new() -> Self {
        let mut ops = Assembler::new().unwrap();

        dynasm!(ops
            ; -> sign_bit:
            ; .qword SIGN_BIT as _

            ; -> qnan:
            ; .qword QNAN as _

            ; -> tag_obj:
            ; .qword (SIGN_BIT | QNAN) as _

            ; -> tag_obj_not:
            ; .qword !(SIGN_BIT | QNAN) as _

            ; -> print_value:
            ; .qword print_value as _

            ; -> expected_number:
            ;; handle_error!(ops, expected_number)

            ; -> expected_add_operands:
            ;; handle_error!(ops, expected_numbers_or_strings)

            ; -> expected_callable:
            ;; handle_error!(ops, expected_callable)

            ; -> expected_numbers:
            ;; handle_error!(ops, expected_numbers)

            ; -> uninit_global:
            ;; handle_error!(ops, uninit_global)

            ; -> fn_arity_mismatch:
            ;; handle_error!(ops, fn_arity_mismatch)

            ; -> stack_overflow:
            ;; handle_error!(ops, stack_overflow)

            ; -> field_requires_instance:
            ;; handle_error!(ops, field_requires_instance)

            ; -> property_requires_instance:
            ;; handle_error!(ops, property_requires_instance)

            ; -> uninit_field:
            ;; handle_error!(ops, uninit_field)

            ; -> super_must_be_class:
            ;; handle_error!(ops, super_must_be_class)

            ; ->exit_error:
            ; mov r15, [r12]
            ; mov r14, [r12+0x8]
            ; mov r13, [r12+0x10]
            ; mov rsi, [r12+0x20]
            ; mov rdi, [r12+0x28]
            ; mov rbp, [r12+0x30]
            ; mov rbx, [r12+0x38]
            ; mov rsp, [r12+0x40]
            ; mov r12, [r12+0x18]
            ; xor rax, rax
            ; ret

            ; ->exit_success:
            ; mov r15, [r12]
            ; mov r14, [r12+0x8]
            ; mov r13, [r12+0x10]
            ; mov rsi, [r12+0x20]
            ; mov rdi, [r12+0x28]
            ; mov rbp, [r12+0x30]
            ; mov rbx, [r12+0x38]
            ; mov rsp, [r12+0x40]
            ; mov r12, [r12+0x18]
            ; mov rax, 1
            ; ret
        );

        let start = ops.offset();

        entrypoint_prologue(&mut ops);

        unsafe {
            GLOBALS.clear();
            GLOBALS.extend((0..RESERVED_GLOBAL_VARS).map(|_| 0))
        }
        Self { start, ops }
    }

    pub fn add_global(&mut self, name: GcCell<ObjString>) -> GlobalVarIndex {
        if unsafe { GLOBALS.len() } * 8 > i32::MAX as usize {
            panic!("Too many globals");
        }
        unsafe { GLOBALS.push(UNINIT_VAL.to_bits()) };
        unsafe { GLOBALS_NAMES.push(name) };
        GlobalVarIndex((unsafe { GLOBALS.len() } - 1) as i32 * 8)
    }

    pub fn get_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; mov rax, QWORD [r12 + index.0]
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rax, rcx
            ; jne >ok
            ; mov r8, index.0
            ; call ->uninit_global
            ; ok:
            ; push rax
        )
    }

    pub fn set_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rcx, [r12 + index.0]
            ; jne >ok
            ; mov r8, index.0
            ; call ->uninit_global
            ; ok:
            ; mov rax, [rsp]
            ; mov [r12 + index.0], rax
        )
    }

    pub fn builtin_fn_0(&mut self, builtin_fn: extern "win64" fn() -> Value) {
        dynasm!(self.ops
            ; jmp >over
            ; builtin_fn:
            ;; call_extern!(self.ops, builtin_fn)
            ; ret
            ; over:

            ; push rbp
            ; push rsi
            ; push null_mut::<ObjString>() as i32
            ; lea rcx, [<builtin_fn]
            ; push rcx
            ; mov rcx, rsp
            ; mov rdx, 0
            ;; call_extern!(self.ops, alloc_closure)
            ; add rsp, 0x20
            ; push rax
        );
    }

    pub fn builtin_fn_1(&mut self, builtin_fn: extern "win64" fn(arg0: Value) -> Value) {
        dynasm!(self.ops
            ; jmp >over
            ; builtin_fn:
            ; mov rcx, [rsp + 0x10]
            ;; call_extern!(self.ops, builtin_fn)
            ; ret
            ; over:

            ; push rbp
            ; push rsi
            ; push null_mut::<ObjString>() as i32
            ; lea rcx, [<builtin_fn]
            ; push rcx
            ; mov rcx, rsp
            ; mov rdx, 0
            ;; call_extern!(self.ops, alloc_closure)
            ; add rsp, 0x20
            ; push rax
        );
    }

    pub fn define_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; pop rax
            ; mov [r12 + index.0], rax
        )
    }

    pub fn set_local(&mut self, index: usize) {
        dynasm!(self.ops
            ; mov rax, [rsp]
            ; mov QWORD [rbp - (index as i32 * 8)], rax
        )
    }

    pub fn get_local(&mut self, index: usize) {
        dynasm!(self.ops
            ; push QWORD [rbp - (index as i32 * 8)]
        )
    }

    pub fn set_upvalue(&mut self, index: usize) {
        dynasm!(self.ops
            // rax: ptr to GcCell<Upvalue>s
            ; mov rax, [rsi + 0x10]

            // rax: ptr to Upvalue
            ; mov rax, [rax + 0x8 * index as i32]

            // rax: ptr to actual value
            ; mov rax, [rax + 0x8]

            ; mov rcx, [rsp]
            ; mov [rax], rcx
        )
    }

    pub fn get_upvalue(&mut self, index: usize) {
        dynasm!(self.ops
            // rax: ptr to GcCell<Upvalue>s
            ; mov rax, [rsi + 0x10]

            // rax: ptr to Upvalue
            ; mov rax, [rax + 0x8 * index as i32]

            // rax: ptr to actual value
            ; mov rax, [rax + 0x8]

            ; push QWORD [rax]
        )
    }

    fn push_const(&mut self, constant: u64) {
        dynasm!(self.ops
            ; mov rax, QWORD constant as _
            ; push rax
        )
    }

    pub fn value(&mut self, value: Value) {
        self.push_const(value.to_bits() as _);
    }
    pub fn nil(&mut self) {
        my_dynasm!(self.ops,
            ; push nil_val
        )
    }
    pub fn true_(&mut self) {
        my_dynasm!(self.ops,
            ; push true_val
        )
    }
    pub fn false_(&mut self) {
        my_dynasm!(self.ops,
            ; push false_val
        )
    }
    pub fn number(&mut self, number: f64) {
        self.push_const(number.to_bits());
    }
    pub fn negate(&mut self) {
        dynasm!(self.ops
            ; pop rax
            ; mov rcx, rax
            ; and rcx, [->qnan]
            ; cmp rcx, [->qnan]
            ; jne >ok
            ; call ->expected_number
            ; ok:
            ; xor rax, [->sign_bit]
            ; push rax
        )
    }
    pub fn add(&mut self) {
        dynasm!(self.ops
            ; mov rax, [rsp]
            ; movq xmm1, rax
            ; and rax, [->qnan]
            ; cmp rax, [->qnan]
            ; je >add_strings

            ; mov rcx, [rsp+8]
            ; movq xmm0, rcx
            ; and rcx, [->qnan]
            ; cmp rcx, [->qnan]
            ; je >add_strings

            ; add rsp, 8
            ; addsd xmm0, xmm1
            ; movq [rsp], xmm0
            ; jmp >end

            ; add_strings:
            ; mov rdx, [rsp]
            ; mov rcx, [rsp+8]

            ; mov rax, rcx
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne >fail

            ; and rcx, [->tag_obj_not]
            ; cmp [rcx], BYTE ObjType::String as _
            ; jne >fail

            ; mov rax, rdx
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne >fail

            ; and rdx, [->tag_obj_not]
            ; cmp [rdx], BYTE ObjType::String as _
            ; jne >fail

            ; add rsp, 8
            ;; call_extern!(self.ops, concat_strings)
            ; mov [rsp], rax
            ; jmp >end

            ; fail:
            ; call ->expected_add_operands

            ; end:
        );
    }
    pub fn sub(&mut self) {
        self.numeric_binary(BinaryOp::Sub)
    }
    pub fn mul(&mut self) {
        self.numeric_binary(BinaryOp::Mul)
    }
    pub fn div(&mut self) {
        self.numeric_binary(BinaryOp::Div)
    }
    fn numeric_binary(&mut self, op: BinaryOp) {
        dynasm!(self.ops
            ; mov rax, [rsp]
            ; movq xmm1, rax
            ; and rax, [->qnan]
            ; cmp rax, [->qnan]
            ; je >fail

            ; mov rcx, [rsp+8]
            ; movq xmm0, rcx
            ; and rcx, [->qnan]
            ; cmp rcx, [->qnan]
            ; jne >ok

            ; fail:
            ; call ->expected_numbers

            ; ok:
            ; add rsp, 8
        );
        match op {
            BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Less | BinaryOp::LessEqual => {
                dynasm!(self.ops
                    ; ucomisd xmm0, xmm1
                )
            }
            _ => {}
        }

        match op {
            BinaryOp::Sub => dynasm!(self.ops
                ; subsd xmm0, xmm1
            ),
            BinaryOp::Mul => dynasm!(self.ops
                ; mulsd xmm0, xmm1
            ),
            BinaryOp::Div => dynasm!(self.ops
                ; divsd xmm0, xmm1
            ),
            BinaryOp::Greater => {
                my_dynasm!(self.ops,
                    ; mov rax, false_val
                    ; cmovnc rax, true_val
                    ; cmove rax, false_val
                )
            }
            BinaryOp::Less => {
                my_dynasm!(self.ops,
                    ; mov rax, false_val
                    ; cmovc rax, true_val
                    ; cmove rax, false_val
                )
            }
            BinaryOp::GreaterEqual => {
                my_dynasm!(self.ops,
                    ; mov rax, false_val
                    ; cmovnc rax, true_val
                )
            }
            BinaryOp::LessEqual => {
                my_dynasm!(self.ops,
                    ; mov rax, false_val
                    ; cmovc rax, true_val
                    ; cmove rax, true_val
                    ; cmovp rax, false_val
                )
            }
        }
        match op {
            BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                dynasm!(self.ops
                    ; movq [rsp], xmm0
                )
            }
            BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Less | BinaryOp::LessEqual => {
                dynasm!(self.ops
                    ; mov [rsp], rax
                )
            }
        }
    }

    pub fn eq(&mut self) {
        eq!(self.ops, true_val, false_val);
    }
    pub fn ne(&mut self) {
        eq!(self.ops, false_val, true_val);
    }
    pub fn gt(&mut self) {
        self.numeric_binary(BinaryOp::Greater)
    }
    pub fn ge(&mut self) {
        self.numeric_binary(BinaryOp::GreaterEqual)
    }
    pub fn lt(&mut self) {
        self.numeric_binary(BinaryOp::Less)
    }
    pub fn le(&mut self) {
        self.numeric_binary(BinaryOp::LessEqual)
    }

    pub fn not(&mut self) {
        my_dynasm!(self.ops,
            ; mov rax, [rsp]
            ; cmp rax, false_val
            ; cmove rax, true_val
            ; je >end
            ; cmp rax, nil_val
            ; cmove rax, true_val
            ; je >end
            ; mov rax, false_val
            ; end:
            ; mov [rsp], rax
        )
    }

    pub fn start_fn(&mut self, arity: u8) -> FnInfo {
        let start = self.ops.new_dynamic_label();
        dynasm!(self.ops
            ; =>start
            ; cmp rcx, arity as _
            ; je >ok
            ; mov r8, rcx
            ; mov r9, arity as _
            // Jump instead of calling to make the error appear on the call site.
            ; jmp ->fn_arity_mismatch
            ; ok:
            ; inc edi
            ; cmp edi, FRAMES_MAX as _
            ; je ->stack_overflow
            ; push rbp
            ; lea rbp, [rsp + (arity * 8 + 0x18) as _]
        );
        FnInfo {
            arity,
            start,
            epilogue: self.ops.new_dynamic_label(),
        }
    }
    pub fn fn_epilogue(&mut self, fn_info: FnInfo) {
        dynasm!(self.ops
            ; => fn_info.epilogue
            ; mov rcx, rbp
            ;; call_extern!(self.ops, close_upvalues)
            ; pop rax
            ; lea rsp, [rbp - ((fn_info.arity * 8 + 0x18) as i32)]
            ; pop rbp
            ; dec edi
            ; ret
        )
    }
    pub fn close_upvalue(&mut self) {
        dynasm!(self.ops
            ; mov rcx, rsp
            ;; call_extern!(self.ops, close_upvalues)
            ; add rsp, 0x8
        );
    }
    pub fn end_fn(
        &mut self,
        fn_info: FnInfo,
        name: GcCell<ObjString>,
        upvalues: impl ExactSizeIterator<Item = (bool, usize)>
            + DoubleEndedIterator<Item = (bool, usize)>,
    ) {
        let len = upvalues.len();
        for (is_local, index) in upvalues.rev() {
            dynasm!(self.ops
                ; push is_local as _
                ; push index as _
            )
        }
        dynasm!(self.ops
            ; push rbp
            ; push rsi
            ; mov rcx, QWORD name.to_bits() as _
            ; push rcx
            ; lea rcx, [=>fn_info.start]
            ; push rcx
            ; mov rcx, rsp
            ; mov rdx, len as _
            ;; call_extern!(self.ops, alloc_closure)
            ; add rsp, 0x20 + 0x10 * len as i32
            ; push rax
        )
    }
    pub fn ret(&mut self, fn_info: FnInfo) {
        dynasm!(self.ops
            ; jmp =>fn_info.epilogue
        )
    }
    pub fn call(&mut self, arity: u8) {
        dynasm!(self.ops
            ; mov rax, [rsp + (arity * 8) as _]

            ; mov rdx, rax
            ; and rdx, [->tag_obj]
            ; cmp rdx, [->tag_obj]
            ; jne >fail

            ; and rax, [->tag_obj_not]
            ; cmp [rax], BYTE ObjType::Closure as _
            ; je >call_closure

            ; cmp [rax], BYTE ObjType::BoundMethod as _
            ; je >call_bound_method

            ; cmp [rax], BYTE ObjType::Class as _
            ; jne >fail
            ; mov rcx, rax
            // preserve rcx on the stack
            ; push rcx
            ;; call_extern!(self.ops, alloc_instance)
            ; pop rcx
            // move [instance] to parameter 0 position
            ; mov [rsp + (arity * 8) as _], rax
            ; cmp QWORD [rcx + 0x10], 0
            ; je >default_constructor
            ; mov rax, [rcx + 0x10]
            ; push rsi
            ; mov rsi, rax
            ; mov rax, [rax + 8]
            ; mov rcx, arity as _
            ; call rax
            ; pop rsi
            ; add rsp, (arity * 8) as _
            ; jmp >ok

            // check that there are exactly 0 arguments
            ; default_constructor:
            ;; if arity == 0 {
                dynasm!(self.ops
                    ; jmp >ok
                )
            } else {
                dynasm!(self.ops
                    ; mov r8, arity as _
                    ; xor r9, r9
                    ; call ->fn_arity_mismatch
                )
            }

            ; call_bound_method:
            // [slot 0] <- receiver
            ; mov rdx, [rax + 8]
            ; mov [rsp + (arity * 8) as _], rdx
            // rax <- ObjClosure
            ; mov rax, [rax + 0x10]

            ; call_closure:
            ; push rsi
            ; mov rsi, rax
            ; mov rax, [rax + 8]
            ; mov rcx, arity as _
            ; call rax
            ; pop rsi
            ; add rsp, (arity * 8) as _
            ; mov [rsp], rax
            ; jmp >ok

            ; fail:
            ; call ->expected_callable
            ; ok:
        )
    }

    pub fn invoke(&mut self, name: GcCell<ObjString>, arity: u8) {
        //let check_offsets;
        dynasm!(self.ops
            ; mov rcx, [rsp + (arity * 8) as _]

            ; mov r8, rcx
            ; and r8, [->tag_obj]
            ; cmp r8, [->tag_obj]
            ; jne >fail2

            ; and rcx, [->tag_obj_not]

            ;; let check_offsets = invoke_ic!(self.ops)

            ; mov rdx, QWORD name.to_bits() as _

            ;; call_extern!(self.ops, invoke, check_offsets, [INVOKE_IC_SHAPE_OFFSET, INVOKE_IC_METHOD_OFFSET])
            ; cmp rax, 0
            ; je >unhappy_path

            ; after_resolve_method:
            ; mov rcx, arity as _

            ; push rsi
            ; mov rsi, rax
            ; mov rax, [rax + 8]
            ; call rax
            ; pop rsi
            ; add rsp, (arity * 8) as _
            ; mov [rsp], rax
            ; jmp >ok2

            ; unhappy_path:
            ;; self.get_property_with_stack_offset(name, arity as i32 * 8)
            ;; self.call(arity)
            ; jmp >ok2

            ; fail2:
            ; call ->expected_callable
            ; ok2:
        );
    }

    pub fn push_class(&mut self, name: GcCell<ObjString>) {
        dynasm!(self.ops
            ; mov rcx, QWORD name.to_bits() as _
            ;; call_extern!(self.ops, alloc_class)
            ; push rax
        )
    }

    pub fn get_property(&mut self, name: GcCell<ObjString>) {
        self.get_property_with_stack_offset(name, 0)
    }

    /// Gets the property `name` from the instance at [rsp + stack_offset] and writes the result to that location.
    fn get_property_with_stack_offset(&mut self, name: GcCell<ObjString>, stack_offset: i32) {
        let check_offsets;
        dynasm!(self.ops
            ; mov rcx, [rsp + stack_offset]

            // TODO: common check
            ; mov r8, rcx
            ; and r8, [->tag_obj]
            ; cmp r8, [->tag_obj]
            ; jne >fail

            ; and rcx, [->tag_obj_not]
            ; cmp [rcx], BYTE ObjType::Instance as _
            ; jne >fail
            ;; check_offsets = get_property_ic!(self.ops, stack_offset)

            ; mov rdx, QWORD name.to_bits() as _
            ;; call_extern!(self.ops, get_property, check_offsets, [GET_PROP_IC_SHAPE_OFFSET, GET_PROP_IC_INDEX_OFFSET])
            ; mov [rsp + stack_offset], rax
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rax, rcx
            ; jne >ok
            ; mov r8, QWORD name.to_bits() as _
            ; call ->uninit_field
            ; jmp >ok

            ; fail:
            ; call ->property_requires_instance

            ; ok:
        )
    }

    pub fn set_property(&mut self, name: GcCell<ObjString>) {
        let check_offsets;

        dynasm!(self.ops
            ; mov rcx, [rsp + 0x8]

            // TODO: common check
            ; mov r8, rcx
            ; and r8, [->tag_obj]
            ; cmp r8, [->tag_obj]
            ; jne >fail

            ; and rcx, [->tag_obj_not]
            ; cmp [rcx], BYTE ObjType::Instance as _
            ; jne >fail
            ;; check_offsets = set_property_ic!(self.ops)

            ; mov rdx, QWORD name.to_bits() as _
            ;; call_extern!(
                self.ops,
                set_property,
                check_offsets,
                [
                    SET_PROP_IC_SHAPE_OFFSET,
                    SET_PROP_IC_INDEX_OFFSET,
                    SET_PROP_IC_OFFSET,
                    SET_PROP_IC_END_JMP_OFFSET,
                ]
            )
            ; pop rcx
            ; mov [rax], rcx
            ; add rsp, 0x8
            ; push rcx

            ; jmp >ok

            ; fail:
            ; call ->field_requires_instance

            ; ok:
        )
    }

    pub fn add_method(&mut self, name: GcCell<ObjString>) {
        dynasm!(self.ops
            ; mov rcx, [rsp + 0x8]
            ; pop rdx
            ; mov r8, QWORD name.to_bits() as _
            ;; call_extern!(self.ops, add_method)
        )
    }

    pub fn inherit(&mut self) {
        dynasm!(self.ops
            ; pop rdx
            ; mov rcx, [rsp]

            ; mov rax, rcx
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne >super_not_class
            ; and rcx, [->tag_obj_not]
            ; cmp [rcx], BYTE ObjType::Class as _
            ; jne >super_not_class

            ;; call_extern!(self.ops, inherit)
            ; jmp >ok

            ; super_not_class:
            ; call ->super_must_be_class

            ; ok:
        )
    }

    pub fn get_super(&mut self, name: GcCell<ObjString>) {
        dynasm!(self.ops
            ; pop rcx
            ; and rcx, [->tag_obj_not]
            ; mov rdx, QWORD name.to_bits() as _
            ;; call_extern!(self.ops, get_super)
            ; mov [rsp], rax
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rax, rcx
            ; jne >ok
            ; mov r8, QWORD name.to_bits() as _
            ; call ->uninit_field

            ; ok:
        )
    }

    pub fn invoke_super(&mut self, name: GcCell<ObjString>, arity: u8) {
        dynasm!(self.ops
            ; pop rcx
            ; and rcx, [->tag_obj_not]

            ; mov rdx, QWORD name.to_bits() as _

            ;; call_extern!(self.ops, invoke_super)
            ; cmp rax, 0
            ; je >unhappy_path

            ; mov rcx, arity as _

            ; push rsi
            ; mov rsi, rax
            ; mov rax, [rax + 8]
            ; call rax
            ; pop rsi
            ; add rsp, (arity * 8) as _
            ; mov [rsp], rax
            ; jmp >ok

            ; unhappy_path:
            ; mov r8, QWORD name.to_bits() as _
            ; call ->uninit_field

            ; ok:
        );
    }

    pub fn get_new_label(&mut self) -> DynamicLabel {
        self.ops.new_dynamic_label()
    }
    pub fn jump(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; jmp =>label
        );
    }
    pub fn jump_if_false(&mut self, label: DynamicLabel) {
        my_dynasm!(self.ops,
            ; mov rax, [rsp]
            ; cmp rax, false_val
            ; je =>label
            ; cmp rax, nil_val
            ; je =>label
        );
    }
    pub fn jump_if_true(&mut self, label: DynamicLabel) {
        my_dynasm!(self.ops,
            ; mov rax, [rsp]
            ; cmp rax, false_val
            ; je >end
            ; cmp rax, nil_val
            ; je >end
            ; jmp =>label
            ; end:
        );
    }
    pub fn set_jump_target(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; =>label
        )
    }
    pub fn pop(&mut self) {
        dynasm!(self.ops
            ; add rsp, 0x8
        )
    }

    pub fn print(&mut self) {
        dynasm!(self.ops
            ; pop rcx
            ;; call_extern!(self.ops, print_value)
        )
    }

    pub fn set_line(&self, line: usize) {
        unsafe { &mut SOURCE_MAPPING }.set_line(self.ops.offset(), line)
    }

    pub fn enter_function_scope(&self, name: Option<GcCell<ObjString>>, args_count: u8) {
        unsafe { &mut SOURCE_MAPPING }
            .begin_function(self.ops.offset(), FnSourceInfo::new(name, args_count))
    }

    pub fn run(&mut self) -> bool {
        dynasm!(self.ops
            ; jmp -> exit_success
        );

        self.ops.commit().unwrap();

        let reader = self.ops.reader();
        let reader = reader.lock();

        unsafe { INSTRUCTIONS_BASE_PTR = reader.as_ptr() }

        let fun = unsafe {
            std::mem::transmute::<_, extern "win64" fn(*mut u64) -> bool>(reader.ptr(self.start))
        };
        self.start = self.ops.offset();
        unsafe {
            ASSEMBLY_BASE = reader.ptr(AssemblyOffset(0));
            ASSEMBLER = &mut self.ops as _;
        }

        // Lie that we don't use the memory in 'reader' anymore.
        // As long as we don't reallocate while executing code this is ok.
        drop(reader);

        let result = fun(unsafe { GLOBALS.as_mut_ptr() });

        // compile the prologue for the next time.
        entrypoint_prologue(&mut self.ops);

        result
    }
}

extern "win64" fn print_value(value: Value) {
    if value.to_bits() == FALSE_VAL.to_bits() {
        println!("false")
    } else if value.to_bits() == TRUE_VAL.to_bits() {
        println!("true")
    } else if value.to_bits() == NIL_VAL.to_bits() {
        println!("nil")
    } else if value.is_obj() {
        println!("{}", value.as_obj())
    } else {
        println!("{}", value.as_number())
    }
}

extern "C" {
    #[link_name = "llvm.frameaddress"]
    fn frameaddress(level: i32) -> *const Value;
    #[link_name = "llvm.returnaddress"]
    fn returnaddress(level: i32) -> *const u8;
}

macro_rules! stack {
    () => {
        unsafe {
            // add 0x18 to frameaddress to jump over [the beginning of the current frame], [rip], [a dummy value pushed to align the stack or otherwise rbp]
            let range = frameaddress(0).add(3)..(GLOBALS[8] as *const Value);
            range
         }
    };
}

macro_rules! return_address {
    () => {
        unsafe { returnaddress(0) }
    };
}

macro_rules! asm_offset {
    () => {
        unsafe { returnaddress(0).sub_ptr(ASSEMBLY_BASE) }
    };
}

extern "win64" fn alloc_class(name: GcCell<ObjString>) -> Value {
    let class = ObjClass::new(name);
    Value::from(register_object(class, stack!()))
}

extern "win64" fn alloc_instance(class: GcCell<ObjClass>) -> Value {
    let instance = ObjInstance::new(class);
    Value::from(register_object(instance, stack!()))
}

extern "win64" fn alloc_closure(args_ptr: *mut u8, args_count: i32) -> u64 {
    let instructions_ptr: *const u8 = unsafe { *args_ptr.cast() };
    let name_ptr: *mut ObjString = unsafe { *args_ptr.add(0x8).cast() };
    let closure: GcCell<ObjClosure> = unsafe { *args_ptr.add(0x10).cast() };

    let mut upvalues = Vec::with_capacity(args_count as usize);

    for i in 0..(args_count as usize) {
        let base_ptr = unsafe { *args_ptr.add(0x18).cast::<*mut u8>() };
        let index = unsafe { *args_ptr.add(0x20 + i * 0x10).cast::<i32>() };
        let is_local = unsafe { *args_ptr.add(0x28 + i * 0x10).cast::<i32>() } != 0;

        upvalues.push(if is_local {
            let upvalue = capture_upvalue(
                unsafe { base_ptr.sub(index as usize * 0x8).cast() },
                stack!(),
            );
            unsafe {
                // Write the upvalue to the stack (temporarily) so it is not gc'ed
                args_ptr
                    .add(0x20 + i * 0x10)
                    .cast::<Value>()
                    .write(upvalue.into());
            }
            upvalue
        } else {
            unsafe { *closure.upvalues.0.add(index as usize) }
        })
    }

    let closure = ObjClosure::new(
        instructions_ptr,
        if name_ptr.is_null() {
            None
        } else {
            Some(unsafe { GcCell::from_bits(name_ptr as _) })
        },
        upvalues,
    );
    Value::from(register_object(closure, stack!())).to_bits()
}

extern "win64" fn get_property(receiver: GcCell<ObjInstance>, name: GcCell<ObjString>) -> Value {
    let asm_offset = asm_offset!();
    let return_address = return_address!();

    if let Some(entry) = ObjShape::resolve_get_property(receiver.shape, name) {
        match entry {
            ShapeEntry::Present { offset } => {
                if ENABLE_ICS {
                    // Do not recompile the IC if it was compiled previously
                    if unsafe { *return_address.sub(GET_PROP_IC_SHAPE_OFFSET).cast::<usize>() } == 0
                    {
                        unsafe { &mut *ASSEMBLER }
                            .alter(|modifier| {
                                modifier
                                    .goto(AssemblyOffset(asm_offset - GET_PROP_IC_SHAPE_OFFSET));
                                modifier.push_u64(receiver.shape.as_ptr() as u64);
                                modifier
                                    .goto(AssemblyOffset(asm_offset - GET_PROP_IC_INDEX_OFFSET));
                                modifier.push_i32((offset * 8) as i32);
                            })
                            .unwrap();
                    }
                }

                *receiver.fields.get(offset)
            }
            ShapeEntry::Method { closure } => {
                register_object(ObjBoundMethod::new(receiver.into(), closure), stack!()).into()
            }
            _ => unreachable!(),
        }
    } else {
        UNINIT_VAL
    }
}

extern "win64" fn set_property(
    mut receiver: GcCell<ObjInstance>,
    name: GcCell<ObjString>,
) -> *mut Value {
    let return_address = return_address!();
    let asm_offset = asm_offset!();
    let property_len = receiver.fields.len();
    match ObjShape::resolve_set_property(receiver.shape, name, property_len) {
        ShapeEntry::Present { offset } => {
            if ENABLE_ICS {
                // Do not recompile the IC if it was compiled previously
                if unsafe { *return_address.sub(SET_PROP_IC_SHAPE_OFFSET).cast::<usize>() } == 0 {
                    unsafe { &mut *ASSEMBLER }
                        .alter(|modifier| {
                            modifier.goto(AssemblyOffset(asm_offset - SET_PROP_IC_SHAPE_OFFSET));
                            modifier.push_u64(receiver.shape.as_ptr() as u64);
                            modifier.goto(AssemblyOffset(asm_offset - SET_PROP_IC_INDEX_OFFSET));
                            modifier.push_i32((offset * 8) as i32);
                        })
                        .unwrap();
                }
            }

            receiver.fields.get_mut(offset)
        }
        ShapeEntry::MissingWithKnownShape { shape, .. } => {
            if ENABLE_ICS {
                // Do not recompile the IC if it was compiled previously
                if unsafe { *return_address.sub(SET_PROP_IC_SHAPE_OFFSET).cast::<usize>() } == 0 {
                    unsafe { &mut *ASSEMBLER }
                        .alter(|modifier| {
                            modifier.goto(AssemblyOffset(asm_offset - SET_PROP_IC_SHAPE_OFFSET));
                            modifier.push_u64(receiver.shape.as_ptr() as u64);
                            modifier.goto(AssemblyOffset(asm_offset - SET_PROP_IC_OFFSET));
                            let start = modifier.offset().0;
                            dynasm!(modifier
                                ; push rcx
                                ;; call_extern!(modifier, push_property)
                                ; mov r8, QWORD shape.as_ptr() as _
                                ; pop rcx
                                ; mov QWORD [rcx + 0x10], r8
                            );
                            assert_eq!(modifier.offset().0 - start, SET_PROP_IC_LEN);
                        })
                        .unwrap();
                }
            }

            receiver.shape = shape;
            receiver.fields.push(UNINIT_VAL);
            receiver.fields.get_mut(property_len)
        }
        _ => unreachable!(),
    }
}

extern "win64" fn push_property(mut receiver: GcCell<ObjInstance>, value: Value) {
    receiver.fields.push(value);
}

extern "win64" fn inherit(superclass: Value, subclass: Value) {
    let mut subclass = subclass.as_obj_class();
    let superclass = superclass.as_obj_class();
    subclass.shape = register_const_object(ObjShape::clone(&superclass.shape));
    subclass.init = superclass.init;
}

extern "win64" fn get_super(receiver: GcCell<ObjClass>, name: GcCell<ObjString>) -> Value {
    if let Some(entry) = ObjShape::resolve_get_property(receiver.shape, name) {
        match entry {
            ShapeEntry::Present { .. } => UNINIT_VAL,
            ShapeEntry::Method { closure } => {
                register_object(ObjBoundMethod::new(receiver.into(), closure), stack!()).into()
            }
            _ => unreachable!(),
        }
    } else {
        UNINIT_VAL
    }
}

extern "win64" fn invoke(
    receiver: GcCell<ObjInstance>,
    name: GcCell<ObjString>,
) -> *const ObjClosure {
    let return_address = return_address!();
    let asm_offset = asm_offset!();

    match ObjShape::resolve_get_property(receiver.shape, name) {
        Some(ShapeEntry::Method { closure }) => {
            if ENABLE_ICS {
                // Do not recompile the IC if it was compiled previously
                if unsafe { *return_address.sub(INVOKE_IC_SHAPE_OFFSET).cast::<usize>() } == 0 {
                    unsafe { &mut *ASSEMBLER }
                        .alter(|modifier| {
                            modifier.goto(AssemblyOffset(asm_offset - INVOKE_IC_SHAPE_OFFSET));
                            modifier.push_u64(receiver.shape.as_ptr() as u64);
                            modifier.goto(AssemblyOffset(asm_offset - INVOKE_IC_METHOD_OFFSET));
                            modifier.push_u64(closure.as_ptr() as _);
                        })
                        .unwrap();
                }
            }
            closure.as_ptr()
        }
        _ => null(),
    }
}

extern "win64" fn invoke_super(
    receiver: GcCell<ObjClass>,
    name: GcCell<ObjString>,
) -> *const ObjClosure {
    match ObjShape::resolve_get_property(receiver.shape, name) {
        Some(ShapeEntry::Method { closure }) => closure.as_ptr(),
        _ => null(),
    }
}

extern "win64" fn add_method(class: Value, closure: Value, name: GcCell<ObjString>) {
    let mut class = class.as_obj_class();
    let closure = closure.as_obj_closure();
    if name == unsafe { *INIT_STRING } {
        class.init = Some(closure)
    }
    ObjShape::add_method(class.shape, name, closure);
}

extern "win64" fn concat_strings(ptr_a: *const ObjString, ptr_b: *const ObjString) -> u64 {
    let new_str = unsafe {
        let a_str = &(*ptr_a).string;
        let b_str = &(*ptr_b).string;
        let mut new_str = String::with_capacity(a_str.len() + b_str.len());
        new_str.push_str(a_str);
        new_str.push_str(b_str);
        new_str
    };
    Value::from(intern_string(new_str, stack!())).to_bits()
}

extern "win64" fn expected_number(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Operand must be a number.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn expected_numbers(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Operands must be numbers.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn expected_numbers_or_strings(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Operands must be two numbers or two strings.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn uninit_global(ip: *const u8, base_ptr: *const u8, var_offset: usize) {
    let index = (var_offset / 8) - RESERVED_GLOBAL_VARS;
    let name = unsafe { GLOBALS_NAMES[index] };
    eprintln!("Undefined variable '{name}'.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn fn_arity_mismatch(ip: *const u8, base_ptr: *const u8, actual: u8, expected: u8) {
    eprintln!("Expected {expected} arguments but got {actual}.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn expected_callable(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Can only call functions and classes.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn stack_overflow(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Stack overflow.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn field_requires_instance(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Only instances have fields.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn property_requires_instance(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Only instances have properties.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn uninit_field(ip: *const u8, base_ptr: *const u8, name: GcCell<ObjString>) {
    eprintln!("Undefined property '{name}'.");
    print_stacktrace(ip, base_ptr)
}
extern "win64" fn super_must_be_class(ip: *const u8, base_ptr: *const u8) {
    eprintln!("Superclass must be a class.");
    print_stacktrace(ip, base_ptr)
}

static mut OPEN_UPVALUES: Option<GcCell<ObjUpvalue>> = None;

fn capture_upvalue(local: *mut Value, stack: Range<*const Value>) -> GcCell<ObjUpvalue> {
    let mut prev_upvalue = None;
    let mut upvalue = unsafe { OPEN_UPVALUES };
    while let Some(current_upvalue) = upvalue && current_upvalue.location < local {
        prev_upvalue = upvalue;
        upvalue = current_upvalue.next;
    }

    if let Some(upvalue) = upvalue && upvalue.location == local {
        return upvalue;
    }

    let mut created_upvalue = register_object(
        ObjUpvalue {
            header: ObjUpvalue::header(),
            location: local,
            next: None,
            closed: MaybeUninit::uninit(),
        },
        stack,
    );
    created_upvalue.next = upvalue;

    if let Some(prev_upvalue) = &mut prev_upvalue {
        prev_upvalue.next = Some(created_upvalue);
    } else {
        unsafe {
            OPEN_UPVALUES = Some(created_upvalue);
        }
    }

    created_upvalue
}

extern "win64" fn close_upvalues(last: *mut Value) {
    while let Some(upvalue) = &mut unsafe{OPEN_UPVALUES} && upvalue.location <= last {
        let value = unsafe { *upvalue.location };
        upvalue.closed.write(value);
        upvalue.location = upvalue.closed.as_mut_ptr();
        unsafe{ OPEN_UPVALUES = upvalue.next; }
    }
}

static mut SOURCE_MAPPING: SourceMapping = SourceMapping::new();
static mut INSTRUCTIONS_BASE_PTR: *const u8 = std::ptr::null();

fn print_stacktrace(ip: *const u8, base_ptr: *const u8) {
    unsafe { SOURCE_MAPPING.print_stacktrace(INSTRUCTIONS_BASE_PTR, ip, base_ptr) }
}

pub extern "win64" fn clock() -> Value {
    (Instant::now().duration_since(*START).as_micros() as f64 / 1000000f64).into()
}

// Read a byte from stdin
pub extern "win64" fn getc() -> Value {
    // A null byte is treated like an EOF to allow for executing LoxLox inside of LoxLox.
    if let Ok(Some(char)) = INPUT_STREAM.lock().unwrap().read_char() && char != '\0' {
        Value::from(char as u32 as f64)
    } else {
        Value::from(-1f64)
    }
}

// Convert given character code number to a single-character string
pub extern "win64" fn chr(code: Value) -> Value {
    let code = code.as_number();

    Value::from(intern_string(char::from(code as u8).to_string(), stack!()))
}

// Exit with given status code
pub extern "win64" fn exit(code: Value) -> Value {
    std::process::exit(code.as_number() as i32)
}

// Print message string on stderr
pub extern "win64" fn print_error(message: Value) -> Value {
    eprintln!("{}", message.as_obj_string());
    NIL_VAL
}
