use std::mem::MaybeUninit;
use std::ops::Range;

use crate::gc::{intern_string, register_object, GcCell};
use crate::object::{ObjClosure, ObjString, ObjType, ObjUpvalue};
use crate::value::{Value, FALSE_VAL, NIL_VAL, QNAN, SIGN_BIT, TRUE_VAL, UNINIT_VAL};
use dynasmrt::x64::Assembler;
use dynasmrt::{dynasm, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi};
use libc::{sigaction, siginfo_t};
use nix::sys::signal;

static mut GLOBALS: Vec<u64> = Vec::new();

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
            ;; call_extern!(ops, expected_number)
            ; jmp ->exit_error

            ; -> expected_add_operands:
            ;; call_extern!(ops, expected_numbers_or_strings)
            ; jmp ->exit_error

            ; -> expected_callable:
            ;; call_extern!(ops, expected_callable)
            ; jmp ->exit_error

            ; -> expected_numbers:
            ;; call_extern!(ops, expected_numbers)
            ; jmp ->exit_error

            ; -> uninit_global:
            ;; call_extern!(ops, uninit_global)
            ; jmp ->exit_error

            ; -> fn_arity_mismatch:
            ;; call_extern!(ops, fn_arity_mismatch)
            ; jmp ->exit_error

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

        dynasm!(ops
            ; lea rcx, [->exit_error]
            ; mov rdx, rsp
            ;; call_extern!(ops, register_signal_handler)
        );

        unsafe {
            GLOBALS.clear();
            GLOBALS.extend((0..9).map(|_| 0))
        }
        Self { start, ops }
    }

    pub fn add_global(&mut self) -> GlobalVarIndex {
        if unsafe { GLOBALS.len() } * 8 > i32::MAX as usize {
            panic!("Too many globals");
        }
        unsafe { GLOBALS.push(UNINIT_VAL.to_bits()) };
        GlobalVarIndex((unsafe { GLOBALS.len() } - 1) as i32 * 8)
    }

    pub fn get_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; mov rax, QWORD [r12 + index.0]
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rax, rcx
            ; je ->uninit_global
            ; push rax
        )
    }

    pub fn set_global(&mut self, index: GlobalVarIndex) {
        dynasm!(self.ops
            ; mov rcx, QWORD UNINIT_VAL.to_bits() as _
            ; cmp rcx, [r12 + index.0]
            ; je ->uninit_global
            ; mov rax, [rsp]
            ; mov [r12 + index.0], rax
        )
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
            ; je ->expected_number
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
            ; jne ->expected_add_operands

            ; and r8, [->tag_obj_not]
            ; cmp [r8], BYTE ObjType::ObjString as _
            ; jne ->expected_add_operands

            ; mov rax, r9
            ; and rax, [->tag_obj]
            ; cmp rax, [->tag_obj]
            ; jne ->expected_add_operands

            ; and r9, [->tag_obj_not]
            ; cmp [r9], BYTE ObjType::ObjString as _
            ; jne ->expected_add_operands

            ; add rsp, 8
            ;; call_extern_alloc!(self.ops, concat_strings)
            ; mov [rsp], rax

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
            ; je ->expected_numbers

            ; mov rcx, [rsp+8]
            ; movq xmm0, rcx
            ; and rcx, [->qnan]
            ; cmp rcx, [->qnan]
            ; je ->expected_numbers

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
            ; push rbp
            ; lea rbp, [rsp + (arity * 8 + 0x10) as _]
            ; cmp rcx, arity as _
            ; jne ->fn_arity_mismatch
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
            ; lea r8, [=>fn_info.start]
            ; push rbp
            ; push r8
            ; mov r8, rsp
            ; mov r9, len as _
            ;; call_extern_alloc!(self.ops, alloc_closure)
            ; add rsp, 0x10 + 0x10 * len as i32
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
            ; jne ->expected_callable

            ; and rax, [->tag_obj_not]
            ; cmp [rax], BYTE ObjType::ObjClosure as _
            ; jne ->expected_callable

            ; mov rax, [rax + 8]
            ; call rax
            ; add rsp, (arity * 8) as _
            ; mov [rsp], rax
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
            ; pop r11
        )
    }

    pub fn print(&mut self) {
        dynasm!(self.ops
            ; pop rcx
            ;; call_extern!(self.ops, print_value)
        )
    }

    pub fn run(&mut self) -> bool {
        dynasm!(self.ops
            ; jmp -> exit_success
        );

        self.ops.commit().unwrap();
        let fun = unsafe {
            std::mem::transmute::<_, extern "win64" fn(*mut u64) -> bool>(
                self.ops.reader().lock().ptr(self.start),
            )
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

    let mut upvalues = Vec::with_capacity(args_count as usize);

    for i in 0..(args_count as usize) {
        let base_ptr = unsafe { *args_ptr.add(0x8).cast::<*mut u8>() };
        let index = unsafe { *args_ptr.add(0x10 + i * 0x10).cast::<i32>() };
        let is_local = unsafe { *args_ptr.add(0x18 + i * 0x10).cast::<i32>() } != 0;

        upvalues.push(if is_local {
            let upvalue = capture_upvalue(
                unsafe { base_ptr.sub(index as usize * 0x8 + 0x8).cast() },
                stack_start..stack_end,
            );
            unsafe {
                // Write the upvalue to the stack (temporarily) so it is not gc'ed
                args_ptr
                    .add(0x10 + i * 0x10)
                    .cast::<Value>()
                    .write(upvalue.into());
            }
            upvalue
        } else {
            let closure = unsafe { *base_ptr.cast::<Value>() }.as_obj_closure();
            unsafe { *closure.upvalues.0.add(index as usize) }
        })
    }

    let closure = ObjClosure::new(instructions_ptr, upvalues);
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

extern "win64" fn expected_number() {
    eprintln!("Operand must be a number.");
}
extern "win64" fn expected_numbers() {
    eprintln!("Operands must be numbers.");
}
extern "win64" fn expected_numbers_or_strings() {
    eprintln!("Operands must be numbers or strings.");
}
extern "win64" fn uninit_global() {
    eprintln!("Unitialized global variable.");
}
extern "win64" fn fn_arity_mismatch() {
    eprintln!("Function arity mismatch.");
}
extern "win64" fn expected_callable() {
    eprintln!("Can only call functions and classes.");
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

extern "win64" fn register_signal_handler(ptr: *const u8, stack_ptr: *const u8) {
    unsafe {
        VM_EPILOGUE_PTR = Some(ptr);
        VM_SOMEWHERE_ON_THE_STACK_PTR = Some(stack_ptr);

        // Shamelessly copied from https://github.com/matklad/backtrace-on-stack-overflow/blob/master/src/lib.rs
        // Might implode or otherwise fail.
        let buf = Vec::leak(vec![0u128; 4096]);
        let stack = libc::stack_t {
            ss_sp: buf.as_ptr() as *mut libc::c_void,
            ss_flags: 0,
            ss_size: buf.len() * std::mem::size_of::<u128>(),
        };
        let mut old = libc::stack_t {
            ss_sp: std::ptr::null_mut(),
            ss_flags: 0,
            ss_size: 0,
        };
        let ret = libc::sigaltstack(&stack, &mut old);
        assert_eq!(ret, 0, "sigaltstack failed");

        signal::sigaction(
            signal::SIGSEGV,
            &signal::SigAction::new(
                signal::SigHandler::SigAction(handle_sigsegv),
                signal::SaFlags::SA_NODEFER | signal::SaFlags::SA_ONSTACK,
                signal::SigSet::empty(),
            ),
        )
        .unwrap();
    }
}

static mut VM_EPILOGUE_PTR: Option<*const u8> = None;

// This pointer points to *somewhere* around the start of the VM stack.
static mut VM_SOMEWHERE_ON_THE_STACK_PTR: Option<*const u8> = None;

extern "C" fn handle_sigsegv(_: i32, siginfo: *mut siginfo_t, _: *mut libc::c_void) {
    let siginfo = unsafe { *siginfo };
    let fault_address = unsafe { siginfo.si_addr() };
    let offset_from_stack =
        unsafe { VM_SOMEWHERE_ON_THE_STACK_PTR.unwrap() as i64 - fault_address as i64 };

    let stack_size_max = nix::sys::resource::getrlimit(nix::sys::resource::Resource::RLIMIT_STACK)
        .unwrap()
        .0 as i64;

    if !(0..stack_size_max).contains(&offset_from_stack) {
        // It looks like this was not a segfault due to a stack overflow. Probably a bug in the generated code.
        eprintln!("BUG: Segmentation fault.");
        std::process::exit(-1);
    }

    eprintln!("Stack overflow.");

    let epilogue =
        unsafe { std::mem::transmute::<_, extern "win64" fn()>(VM_EPILOGUE_PTR.unwrap()) };
    epilogue();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_nil() {
        let mut emitter = Emitter::new();
        emitter.nil();
        emitter.print();
        assert!(emitter.run());
    }

    #[test]
    fn negate() {
        let mut emitter = Emitter::new();
        emitter.number(0f64);
        emitter.negate();
        emitter.print();
        assert!(emitter.run());
    }

    #[test]
    fn negate_fail() {
        let mut emitter = Emitter::new();
        emitter.nil();
        emitter.negate();
        emitter.print();
        assert!(!emitter.run());
    }
    #[test]
    fn sub() {
        let mut emitter = Emitter::new();
        emitter.nil();
        emitter.number(2f64);
        emitter.sub();
        emitter.print();
        assert!(!emitter.run());
    }
    #[test]
    fn eq() {
        let mut emitter = Emitter::new();
        emitter.number(2f64);
        emitter.number(2f64);
        emitter.ne();
        emitter.print();
        assert!(emitter.run());
    }
    #[test]
    fn if_() {
        let mut emitter = Emitter::new();
        emitter.number(2f64);
        let then = emitter.get_new_label();
        emitter.jump_if_false(then);
        emitter.pop();
        emitter.number(1f64);
        emitter.print();
        emitter.set_jump_target(then);
        emitter.pop();
        assert!(emitter.run());
    }
    #[test]
    fn global_var() {
        let mut emitter = Emitter::new();
        emitter.number(0f64);
        /*  let var = emitter.add_global();
        emitter.set_global(var);
        emitter.get_global(var);*/
        emitter.print();
        assert!(emitter.run());
    }
}
