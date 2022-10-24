use crate::value::{Value, FALSE_VAL, NIL_VAL, QNAN, SIGN_BIT, TRUE_VAL};
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

pub struct Emitter {
    ops: Assembler,
    start: AssemblyOffset,
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
            ; mov rax, QWORD expected_number as _
            ; sub rsp, 0x28
            ; call rax
            ; add rsp, 0x28
            ; jmp ->exit_error


            ; -> expected_add_operands:
            ; mov rax, QWORD expected_numbers as _
            ; sub rsp, 0x28
            ; call rax
            ; add rsp, 0x28
            ; jmp ->exit_error

            ; ->exit_error:
            ; mov rsp, r12
            ; pop rbx
            ; pop rbp
            ; pop rdi
            ; pop rsi
            ; pop r12
            ; pop r13
            ; pop r14
            ; pop r15
            ; xor rax, rax
            ; ret

            ; ->exit_success:
            ; mov rsp, r12
            ; pop rbx
            ; pop rbp
            ; pop rdi
            ; pop rsi
            ; pop r12
            ; pop r13
            ; pop r14
            ; pop r15
            ; mov rax, 1
            ; ret
        );

        let start = ops.offset();

        my_dynasm!(ops,
            ; push r15
            ; push r14
            ; push r13
            ; push r12
            ; push rsi
            ; push rdi
            ; push rbp
            ; push rbx
            ; mov r12, rsp
            ; mov true_val, QWORD TRUE_VAL.to_bits() as _
            ; mov false_val, QWORD FALSE_VAL.to_bits() as _
            ; mov nil_val, QWORD NIL_VAL.to_bits() as _
        );
        Self { start, ops }
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
    pub fn set_jump_target(&mut self, label: DynamicLabel) {
        dynasm!(self.ops
            ; =>label
        )
    }
    pub fn pop(&mut self) {
        dynasm!(self.ops
            ; pop rax
        )
    }

    pub fn print(&mut self) {
        dynasm!(self.ops
            ; pop rcx
            ; sub rsp, BYTE 0x28
            ; call QWORD [->print_value]
            ; add rsp, BYTE 0x28
        )
    }

    pub fn run(mut self) -> bool {
        dynasm!(self.ops
            ; jmp -> exit_success
        );
        let executable_buffer = self.ops.finalize().unwrap();
        let fun = unsafe {
            std::mem::transmute::<_, extern "win64" fn() -> bool>(executable_buffer.ptr(self.start))
        };
        fun()
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
}
