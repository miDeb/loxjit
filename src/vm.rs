use std::mem::MaybeUninit;

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_TRACE_EXECUTION,
    compiler::Parser,
    value::{print_value, Value},
};

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

macro_rules! binary_op {
    ($self:expr, $op:ident, $wrap:expr) => {{
        #[allow(unused)]
        use std::ops::*;
        if !$self.peek(0).is_number() || !$self.peek(1).is_number() {
            runtime_error!($self, "Operands must be numbers.");
            return InterpretResult::RuntimeError;
        }
        let b = $self.pop().as_number();
        let a = $self.pop().as_number();
        $self.push($wrap(a.$op(&b)));
    }};
}

pub struct Vm {
    chunk: Chunk,
    ip: *const u8,
    stack: [MaybeUninit<Value>; 256],
    sp: *mut MaybeUninit<Value>,
}

macro_rules! runtime_error {
    ($vm:expr, $($arg:tt)*) => {{
        eprintln!($($arg)*);

        $vm.runtime_error();
    }};
}

impl Vm {
    pub fn new() -> Self {
        let chunk = Chunk::new();
        Self {
            ip: std::ptr::null(),
            chunk,
            stack: [MaybeUninit::uninit(); 256],
            sp: std::ptr::null_mut(),
        }
    }

    fn read_byte(&mut self) -> u8 {
        unsafe {
            let result = *self.ip;
            self.ip = self.ip.add(1);
            result
        }
    }

    fn read_opcode(&mut self) -> OpCode {
        let byte = self.read_byte();
        debug_assert!(OpCode::try_from(byte).is_ok());
        unsafe { OpCode::from_unchecked(byte) }
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        self.chunk.constants[index]
    }

    fn push(&mut self, value: Value) {
        unsafe {
            (*self.sp).write(value);
            self.sp = self.sp.add(1);
        }
    }

    fn pop(&mut self) -> Value {
        unsafe {
            self.sp = self.sp.sub(1);
            (*self.sp).assume_init()
        }
    }

    fn peek(&self, distance: usize) -> Value {
        unsafe { (*self.sp.sub(1 + distance)).assume_init() }
    }

    fn reset_stack(&mut self) {
        self.sp = self.stack.as_mut_ptr();
    }

    fn runtime_error(&mut self) {
        let instruction = unsafe { self.ip.sub_ptr(self.chunk.code.as_ptr()) } - 1;
        let line = self.chunk.lines[instruction];
        eprintln!("[line {line}] in script");
        self.reset_stack();
    }

    pub fn run(&mut self) -> InterpretResult {
        self.reset_stack();
        self.ip = self.chunk.code.as_ptr();
        loop {
            if DEBUG_TRACE_EXECUTION {
                print!("          ");
                let mut slot = self.stack.as_ptr();
                while slot < self.sp {
                    print!("[ ");
                    print_value(unsafe { (*slot).assume_init() });
                    unsafe { slot = slot.add(1) };
                    print!(" ]");
                }
                println!();
                self.chunk
                    .disassemble_instruction(unsafe { self.ip.sub_ptr(self.chunk.code.as_ptr()) });
            }
            match self.read_opcode() {
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Negate => {
                    if !self.peek(0).is_number() {
                        runtime_error!(self, "Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                    let value = self.pop().as_number();
                    self.push(Value::Number(-value));
                }
                OpCode::Return => {
                    print_value(self.pop());
                    println!();
                    return InterpretResult::Ok;
                }
                OpCode::Add => binary_op!(self, add, Value::Number),
                OpCode::Sub => binary_op!(self, sub, Value::Number),
                OpCode::Mul => binary_op!(self, mul, Value::Number),
                OpCode::Div => binary_op!(self, div, Value::Number),
                OpCode::Equal => {
                    let eq = self.pop() == self.pop();
                    self.push(Value::Bool(eq))
                }
                OpCode::Less => binary_op!(self, lt, Value::Bool),
                OpCode::Greater => binary_op!(self, gt, Value::Bool),
                OpCode::Not => {
                    let is_falsey = self.pop().is_falsey();
                    self.push(Value::Bool(is_falsey))
                }
            }
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        self.chunk = Chunk::new();
        let mut parser = Parser::new(source, &mut self.chunk);
        if let Err(()) = parser.compile() {
            return InterpretResult::CompileError;
        }

        self.run()
    }
}
