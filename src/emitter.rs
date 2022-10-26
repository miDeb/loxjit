use crate::value::{Value, FALSE_VAL, NIL_VAL, QNAN, SIGN_BIT, TRUE_VAL, UNINIT_VAL};
use dynasmrt::x64::Assembler;
use dynasmrt::{dynasm, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi};

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

enum BinaryOp {
    Add,
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

pub struct Emitter {
    ops: Assembler,
    start: AssemblyOffset,
    globals: Vec<u64>,
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

            ; -> print_value:
            ; .qword print_value as _

            ; -> expected_number:
            ;; call_extern!(ops, expected_number)
            ; jmp ->exit_error

            ; -> expected_add_operands:
            ;; call_extern!(ops, expected_numbers)
            ; jmp ->exit_error

            ; -> uninit_global:
            ;; call_extern!(ops, uninit_global)
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
            // occupy the slot for "this" (the actual value does not matter)
            ; push rax
        );
        Self {
            start,
            ops,
            globals: vec![0; 9],
        }
    }

    pub fn add_global(&mut self) -> GlobalVarIndex {
        if self.globals.len() * 8 > i32::MAX as usize {
            panic!("Too many globals");
        }
        self.globals.push(UNINIT_VAL.to_bits());
        GlobalVarIndex((self.globals.len() - 1) as i32 * 8)
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
            ; mov rax, [rsp]
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

    fn push_const(&mut self, constant: u64) {
        dynasm!(self.ops
            ; mov rax, QWORD constant as _
            ; push rax
        )
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
        self.numeric_binary(BinaryOp::Add)
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
            ; je ->expected_add_operands

            ; mov rcx, [rsp+8]
            ; movq xmm0, rcx
            ; and rcx, [->qnan]
            ; cmp rcx, [->qnan]
            ; je ->expected_add_operands

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
            BinaryOp::Add => dynasm!(self.ops
                ; addsd xmm0, xmm1
            ),
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
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
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

    pub fn run(mut self) -> bool {
        dynasm!(self.ops
            ; jmp -> exit_success
        );
        let executable_buffer = self.ops.finalize().unwrap();
        let fun = unsafe {
            std::mem::transmute::<_, extern "win64" fn(*mut u64) -> bool>(
                executable_buffer.ptr(self.start),
            )
        };
        fun(self.globals.as_mut_ptr())
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

extern "win64" fn expected_number() {
    eprintln!("Operand must be a number.");
}
extern "win64" fn expected_numbers() {
    eprintln!("Operands must be numbers.");
}
extern "win64" fn uninit_global() {
    eprintln!("Unitialized global variable.");
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
