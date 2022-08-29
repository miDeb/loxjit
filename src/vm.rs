use std::{collections::HashMap, mem::MaybeUninit};

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_TRACE_EXECUTION,
    compiler::Parser,
    interned_strings::StringInterner,
    value::Value,
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
    interned_strings: StringInterner,
    globals: HashMap<&'static str, Value>,
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
            interned_strings: StringInterner::new(),
            globals: HashMap::new(),
        }
    }

    fn read_byte(&mut self) -> u8 {
        unsafe {
            let result = *self.ip;
            self.ip = self.ip.add(1);
            result
        }
    }

    fn read_u16(&mut self) -> u16 {
        u16::from_ne_bytes([self.read_byte(), self.read_byte()])
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

    fn read_string(&mut self) -> &'static str {
        self.read_constant().as_string()
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

    fn concatenate(&mut self) {
        let b = self.pop().as_string();
        let a = self.pop().as_string();

        let mut new_string = String::with_capacity(a.len() + b.len());
        new_string.push_str(a);
        new_string.push_str(b);

        let obj = self.interned_strings.put(new_string);

        self.push(Value::Obj(obj));
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
                    print!("{}", unsafe { (*slot).assume_init() });
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
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.push(unsafe { self.stack[slot as usize].assume_init() });
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = MaybeUninit::new(self.peek(0));
                }
                OpCode::GetGlobal => {
                    let name = self.read_string();
                    let Some(value) = self.globals.get(name) else {
                        runtime_error!(self, "Undefined variable '{}'.", name);
                        return InterpretResult::RuntimeError;
                    };
                    self.push(*value);
                }
                OpCode::DefineGlobal => {
                    let name = self.read_string();
                    self.globals.insert(name, self.peek(0));
                    self.pop();
                }
                OpCode::SetGlobal => {
                    let name = self.read_string();
                    if self.globals.insert(name, self.peek(0)).is_none() {
                        self.globals.remove(name);
                        runtime_error!(self, "Undefined variable '{}'.", name);
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Negate => {
                    if !self.peek(0).is_number() {
                        runtime_error!(self, "Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                    let value = self.pop().as_number();
                    self.push(Value::Number(-value));
                }
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Jump => {
                    let offset = self.read_u16();
                    self.ip = unsafe { self.ip.add(offset as usize) };
                }
                OpCode::JumpUp => {
                    let offset = self.read_u16();
                    self.ip = unsafe { self.ip.sub(offset as usize) };
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_u16();
                    if self.peek(0).is_falsey() {
                        self.ip = unsafe { self.ip.add(offset as usize) };
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.read_u16();
                    if !self.peek(0).is_falsey() {
                        self.ip = unsafe { self.ip.add(offset as usize) };
                    }
                }
                OpCode::Return => {
                    return InterpretResult::Ok;
                }
                OpCode::Add => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate();
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        binary_op!(self, add, Value::Number)
                    } else {
                        runtime_error!(self, "Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                }
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
        let mut parser = Parser::new(source, &mut self.chunk, &mut self.interned_strings);
        if let Err(()) = parser.compile() {
            return InterpretResult::CompileError;
        }

        self.run()
    }
}
