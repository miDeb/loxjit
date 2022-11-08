use std::mem::MaybeUninit;
use std::ops::Range;
use std::ptr::null_mut;
use std::time::Instant;

use crate::gc::{intern_string, register_object, GcCell};
use crate::object::{ObjClosure, ObjString, ObjType, ObjUpvalue};
use crate::source_mapping::{FnSourceInfo, SourceMapping};
use crate::value::{Value, FALSE_VAL, NIL_VAL, QNAN, SIGN_BIT, TRUE_VAL, UNINIT_VAL};
use crate::START;
use dynasmrt::x64::Assembler;
use dynasmrt::{dynasm, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi};

static mut GLOBALS: Vec<u64> = Vec::new();
static mut GLOBALS_NAMES: Vec<GcCell<ObjString>> = Vec::new();
const RESERVED_GLOBAL_VARS: usize = 9;

const FRAMES_MAX: u8 = 64;

pub fn global_vars() -> &'static [Value] {
    unsafe { std::mem::transmute(&GLOBALS[9..]) }
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

macro_rules! call_extern {
    ($ops:expr, $fun:expr) => {
        dynasm!($ops
            // trigger a stack overflow now if there is little stack left
            ; mov rax, [rsp - 0x400]

            ; push rbp
            ; mov rbp, rsp
            ; and spl, BYTE 0xF0 as _

            ; mov rax, QWORD $fun as _
            ; call rax
            ; mov rsp, rbp
            ; pop rbp
        )
    };
}

macro_rules! call_extern_alloc {
    ($ops:expr, $fun:expr) => {
        dynasm!($ops
            // trigger a stack overflow now if there is little stack left
            ; mov rax, [rsp - 0x400]

            ; push rbp
            ; mov rbp, rsp
            ; mov rdx, [r12+0x40]
            ; mov rcx, rsp
            ; and spl, BYTE 0xF0 as _

            ; mov rax, QWORD $fun as _
            ; call rax
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
            ; -> restore_rsp:
            ; .qword 0

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

    pub fn clock(&mut self) {
        dynasm!(self.ops
            ; jmp >over
            ; clock:
            ;; call_extern!(self.ops, clock)
            ; ret
            ; over:

            ; push rbp
            ; push null_mut::<ObjString>() as i32
            ; lea r8, [<clock]
            ; push r8
            ; mov r8, rsp
            ; mov r9, 0
            ;; call_extern_alloc!(self.ops, alloc_closure)
            ; add rsp, 0x18
            ; push rax
        );
    }

    pub fn define_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; pop rax
            ; mov [r12 + index.0], rax
        )
    }

    pub fn set_local(&mut self, index: u8) {
        dynasm!(self.ops
            ; mov rax, [rsp]
            ; mov QWORD [rbp - (index as i32 * 8 + 8)], rax
        )
    }

    pub fn get_local(&mut self, index: u8) {
        dynasm!(self.ops
            ; push QWORD [rbp - (index as i32 * 8 + 8)]
        )
    }

    pub fn set_upvalue(&mut self, index: u8) {
        dynasm!(self.ops
            ; mov rax, [rbp]
            // rax: ptr to ObjClosure
            ; and rax, [->tag_obj_not]

            // rax: ptr to GcCell<Upvalue>s
            ; mov rax, [rax + 0x10]

            // rax: ptr to Upvalue
            ; mov rax, [rax + 0x8 * index as i32]

            // rax: ptr to actual value
            ; mov rax, [rax + 0x8]

            ; mov rcx, [rsp]
            ; mov [rax], rcx
        )
    }

    pub fn get_upvalue(&mut self, index: u8) {
        dynasm!(self.ops
            ; mov rax, [rbp]
            // rax: ptr to ObjClosure
            ; and rax, [->tag_obj_not]

            // rax: ptr to GcCell<Upvalue>s
            ; mov rax, [rax + 0x10]

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
            ; mov r9, [rsp]
            ; mov r8, [rsp+8]

            ; mov rax, r8
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne >fail

            ; and r8, [->tag_obj_not]
            ; cmp [r8], BYTE ObjType::ObjString as _
            ; jne >fail

            ; mov rax, r9
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne >fail

            ; and r9, [->tag_obj_not]
            ; cmp [r9], BYTE ObjType::ObjString as _
            ; jne >fail

            ; add rsp, 8
            ;; call_extern_alloc!(self.ops, concat_strings)
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
            // Jump instead of calling to make the error appear on the call site.
            ; je >ok
            ; mov r8, rcx
            ; mov r9, arity as _
            ; jmp ->fn_arity_mismatch
            ; ok:
            ; inc edi
            ; cmp edi, FRAMES_MAX as _
            ; je ->stack_overflow
            ; push rbp
            ; lea rbp, [rsp + (arity * 8 + 0x10) as _]
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
            ; lea rsp, [rbp - ((fn_info.arity * 8 + 0x10) as i32)]
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
        upvalues: impl ExactSizeIterator<Item = (bool, u8)> + DoubleEndedIterator<Item = (bool, u8)>,
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
            ; mov r8, QWORD name.to_bits() as _
            ; push r8
            ; lea r8, [=>fn_info.start]
            ; push r8
            ; mov r8, rsp
            ; mov r9, len as _
            ;; call_extern_alloc!(self.ops, alloc_closure)
            ; add rsp, 0x18 + 0x10 * len as i32
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
            ; mov rcx, arity as _

            ; mov r8, rax
            ; and r8, [->tag_obj]
            ; cmp r8, [->tag_obj]
            ; jne >fail

            ; and rax, [->tag_obj_not]
            ; cmp [rax], BYTE ObjType::ObjClosure as _
            ; jne >fail

            ; mov rax, [rax + 8]
            ; call rax
            ; add rsp, (arity * 8) as _
            ; mov [rsp], rax
            ; jmp >ok

            ; fail:
            ; call ->expected_callable
            ; ok:
        )
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
        entrypoint_prologue(&mut self.ops);

        fun(unsafe { GLOBALS.as_mut_ptr() })
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

extern "win64" fn alloc_closure(
    stack_start: *const Value,
    stack_end: *const Value,
    args_ptr: *mut u8,
    args_count: i32,
) -> u64 {
    let instructions_ptr: *const u8 = unsafe { *args_ptr.cast() };
    let name_ptr: *mut ObjString = unsafe { *args_ptr.add(0x8).cast() };

    let mut upvalues = Vec::with_capacity(args_count as usize);

    for i in 0..(args_count as usize) {
        let base_ptr = unsafe { *args_ptr.add(0x10).cast::<*mut u8>() };
        let index = unsafe { *args_ptr.add(0x18 + i * 0x10).cast::<i32>() };
        let is_local = unsafe { *args_ptr.add(0x20 + i * 0x10).cast::<i32>() } != 0;

        upvalues.push(if is_local {
            let upvalue = capture_upvalue(
                unsafe { base_ptr.sub(index as usize * 0x8 + 0x8).cast() },
                stack_start..stack_end,
            );
            unsafe {
                // Write the upvalue to the stack (temporarily) so it is not gc'ed
                args_ptr
                    .add(0x18 + i * 0x10)
                    .cast::<Value>()
                    .write(upvalue.into());
            }
            upvalue
        } else {
            let closure = unsafe { *base_ptr.cast::<Value>() }.as_obj_closure();
            unsafe { *closure.upvalues.0.add(index as usize) }
        })
    }

    let closure = ObjClosure::new(
        instructions_ptr,
        if name_ptr == null_mut() {
            None
        } else {
            Some(unsafe { GcCell::from_bits(name_ptr as _) })
        },
        upvalues,
    );
    Value::from(register_object(closure, stack_start..stack_end)).to_bits()
}

extern "win64" fn concat_strings(
    stack_start: *const Value,
    stack_end: *const Value,
    ptr_a: *const ObjString,
    ptr_b: *const ObjString,
) -> u64 {
    let new_str = unsafe {
        let a_str = &(*ptr_a).string;
        let b_str = &(*ptr_b).string;
        let mut new_str = String::with_capacity(a_str.len() + b_str.len());
        new_str.push_str(a_str);
        new_str.push_str(b_str);
        new_str
    };
    Value::from(intern_string(new_str, stack_start..stack_end)).to_bits()
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

extern "win64" fn clock() -> u64 {
    (Instant::now().duration_since(*START).as_micros() as f64 / 1000000f64).to_bits()
}
