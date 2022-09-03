use rustc_hash::FxHashMap;
use std::{mem::MaybeUninit, time::Instant};

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_TRACE_EXECUTION,
    compiler::Parser,
    interned_strings::StringInterner,
    object::{NativeFnRef, ObjClosure, ObjUpvalue},
    value::Value,
    START,
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
    closure: *const ObjClosure,
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
    open_upvalues: Option<*mut ObjUpvalue>,
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
            open_upvalues: None,
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
        &unsafe { &*self.frame().closure }.as_function().chunk
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
            let function = unsafe { &*frame.closure }.as_function();
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
            if callee.is_closure() {
                return self.call(callee.as_closure(), arg_count);
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

    fn call(&mut self, closure: &ObjClosure, arg_count: u8) -> Result<(), ()> {
        let fun = closure.as_function();
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
            closure,
        });
        Ok(())
    }

    fn define_native(&mut self, name: &'static str, fun: NativeFnRef) {
        self.globals.insert(name, fun.into());
    }

    fn capture_upvalue(&mut self, local: *mut Value) -> *const ObjUpvalue {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalues;
        while let Some(current_upvalue) = &mut upvalue && unsafe{&**current_upvalue}.location > local {
            prev_upvalue = Some(*current_upvalue);
            upvalue = unsafe{&**current_upvalue}.next;
        }

        if let Some(upvalue) = upvalue && unsafe{&*upvalue}.location == local {
            return upvalue;
        }

        let created_upvalue = Box::into_raw(Box::new(ObjUpvalue {
            location: local,
            next: None,
            closed: MaybeUninit::uninit(),
        }));
        if let Some(prev_upvalue) = &mut prev_upvalue {
            unsafe { &mut **prev_upvalue }.next = Some(created_upvalue);
        } else {
            self.open_upvalues = Some(created_upvalue);
        }

        created_upvalue
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        while let Some(open_upvalues) = &mut self.open_upvalues && unsafe{&**open_upvalues}.location >= last {
            let mut upvalue = unsafe {&mut **open_upvalues};
            upvalue.closed.write(unsafe{*upvalue.location});
            upvalue.location = (&mut upvalue.closed as *mut MaybeUninit<_>).cast();
            self.open_upvalues = upvalue.next;
        }
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
                OpCode::Closure => {
                    let function = self.read_constant().as_fun();
                    let mut closure: Box<ObjClosure> = Box::new(function.into());
                    for upvalue in closure.upvalues.iter_mut() {
                        let is_local = self.read_byte() != 0;
                        let index = self.read_byte();
                        if is_local {
                            *upvalue = Some(self.capture_upvalue(unsafe {
                                self.frame().slots.add(index as usize).cast()
                            }));
                        } else {
                            *upvalue = unsafe { (*self.frame().closure).upvalues[index as usize] };
                        }
                    }
                    self.push(closure.into());
                }
                OpCode::CloseUpvalue => {
                    let last = unsafe { self.sp.sub(1).cast() };
                    self.close_upvalues(last);
                    self.pop();
                }
                OpCode::Return => {
                    let result = self.pop();
                    self.sp = self.frame().slots;
                    self.close_upvalues(self.sp.cast());
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
                OpCode::GetUpvalue => {
                    let slot = self.read_byte();
                    self.push(unsafe {
                        *(*(*self.frame().closure).upvalues[slot as usize].unwrap_unchecked())
                            .location
                    })
                }
                OpCode::SetUpvalue => {
                    let slot = self.read_byte();
                    unsafe {
                        *(*(*self.frame().closure).upvalues[slot as usize].unwrap_unchecked())
                            .location = self.peek(0);
                    }
                }
            }
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let parser = Parser::new(source, &mut self.interned_strings);
        match parser.compile() {
            Err(_) => InterpretResult::CompileError,
            Ok(function) => {
                self.reset_stack();
                let closure: Box<ObjClosure> = Box::new((&*function).into());
                self.push(closure.into());
                self.call_value(self.peek(0), 0).unwrap();

                self.run()
            }
        }
    }
}
