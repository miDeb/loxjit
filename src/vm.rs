use std::{mem::MaybeUninit, time::Instant};
use rustc_hash::FxHashMap;

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_TRACE_EXECUTION,
    compiler::Parser,
    interned_strings::StringInterner,
    object::{Obj, ObjContents, ObjFunction, NativeFnRef},
    value::Value, START,
};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX + u8::MAX as usize + 1;

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

#[derive(Copy, Clone)]
struct CallFrame {
    function: *const ObjFunction,
    ip: *const u8,
    slots: *mut MaybeUninit<Value>,
}

macro_rules! runtime_error {
    ($vm:expr, $($arg:tt)*) => {{
        eprintln!($($arg)*);

        $vm.runtime_error();
    }};
}


fn clock_native(_args: &[Value]) -> Value {
    Value::Number(Instant::now().duration_since(*START).as_millis() as _)
}

pub struct Vm {
    frames: [MaybeUninit<CallFrame>; FRAMES_MAX],
    frame_count: usize,

    stack: [MaybeUninit<Value>; STACK_MAX],
    sp: *mut MaybeUninit<Value>,
    interned_strings: StringInterner,
    globals: FxHashMap<&'static str, Value>,
}


impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            frame_count: 0,
            frames: [MaybeUninit::uninit(); FRAMES_MAX],
            stack: [MaybeUninit::uninit(); STACK_MAX],
            sp: std::ptr::null_mut(),
            interned_strings: StringInterner::new(),
            globals: FxHashMap::default(),
        };

        vm.define_native("clock", clock_native);
        vm
    }

    fn ip(&mut self) -> &mut *const u8 {
        unsafe { &mut self.frames[self.frame_count - 1].assume_init_mut().ip }
    }

    fn frame(&self) -> &CallFrame {
        unsafe { self.frames[self.frame_count - 1].assume_init_ref() }
    }

    fn push_frame(&mut self, frame: CallFrame) {
        self.frame_count += 1;
        self.frames[self.frame_count - 1] = MaybeUninit::new(frame);
    }

    fn chunk(&self) -> &Chunk {
        &unsafe { &*self.frame().function }.chunk
    }

    fn read_byte(&mut self) -> u8 {
        unsafe {
            let result = **self.ip();
            *self.ip() = self.ip().add(1);
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
        self.chunk().constants[index]
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
        self.frame_count = 0;
    }

    fn runtime_error(&mut self) {
        for i in (0..self.frame_count).rev() {
            let frame = unsafe { self.frames[i].assume_init_ref() };
            let function = unsafe { &*frame.function };
            let instruction = unsafe { frame.ip.sub_ptr(function.chunk.code.as_ptr()) } - 1;
            eprint!("[line {}] in ", function.chunk.lines[instruction]);
            if let Some(name) = &function.name {
                eprintln!("{name}()")
            } else {
                eprintln!("script")
            }
        }

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

    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), ()> {
        if callee.is_obj() {
            if callee.is_fun() {
                return self.call(callee.as_fun(), arg_count);
            } else if callee.is_native_fun() {
                let result = callee.as_native_fun()(unsafe {
                    std::slice::from_raw_parts(
                        self.sp.sub(arg_count as usize) as *const _,
                        arg_count as usize,
                    )
                });
                self.sp = unsafe { self.sp.sub(arg_count as usize + 1) };
                self.push(result);
                return Ok(());
            }
        }
        runtime_error!(self, "Can only call functions and classes.");
        Err(())
    }

    fn call(&mut self, fun: &ObjFunction, arg_count: u8) -> Result<(), ()> {
        if arg_count != fun.arity {
            runtime_error!(
                self,
                "Expected {} arguments but got {}.",
                fun.arity,
                arg_count,
            );
            return Err(());
        }

        if self.frame_count == FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return Err(());
        }

        self.push_frame(CallFrame {
            ip: fun.chunk.code.as_ptr(),
            slots: unsafe { self.sp.sub(arg_count as usize + 1) },
            function: fun,
        });
        Ok(())
    }

    fn define_native(&mut self, name: &'static str, fun: NativeFnRef) {
        self.globals.insert(name, fun.into());
    }

    pub fn run(&mut self) -> InterpretResult {
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
                let offset = unsafe { self.ip().sub_ptr(self.chunk().code.as_ptr()) };
                self.chunk().disassemble_instruction(offset);
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
                    self.push(unsafe { (*self.frame().slots.add(slot as usize)).assume_init() });
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
                    *self.ip() = unsafe { self.ip().add(offset as usize) };
                }
                OpCode::JumpUp => {
                    let offset = self.read_u16();
                    *self.ip() = unsafe { self.ip().sub(offset as usize) };
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_u16();
                    if self.peek(0).is_falsey() {
                        *self.ip() = unsafe { self.ip().add(offset as usize) };
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.read_u16();
                    if !self.peek(0).is_falsey() {
                        *self.ip() = unsafe { self.ip().add(offset as usize) };
                    }
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    if self
                        .call_value(self.peek(arg_count as usize), arg_count)
                        .is_err()
                    {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Return => {
                    let result = self.pop();
                    self.sp = self.frame().slots;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok;
                    }

                    self.push(result);
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
        let parser = Parser::new(source, &mut self.interned_strings);
        match parser.compile() {
            Err(()) => InterpretResult::CompileError,
            Ok(function) => {
                self.reset_stack();
                self.push(function.into());
                self.call_value(self.peek(0), 0).unwrap();

                self.run()
            }
        }
    }
}
