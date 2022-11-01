use num_enum::{IntoPrimitive, TryFromPrimitive, UnsafeFromPrimitive};

use crate::value::Value;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, IntoPrimitive, UnsafeFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    Equal,
    Greater,
    Less,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Negate,
    Print,
    Jump,
    JumpUp,
    JumpIfFalse,
    JumpIfTrue,
    Call,
    Invoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
    GetProperty,
    SetProperty,
    GetSuper,
    SuperInvoke,
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn push(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn push_constant(&mut self, value: Value) -> Result<u8, ()> {
        self.constants.push(value);
        (self.constants.len() - 1).try_into().map_err(|_| ())
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, mut offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let op_code: OpCode = self.code[offset].try_into().unwrap();

        match op_code {
            OpCode::Call => self.byte_instruction("OP_CALL", offset),
            OpCode::Constant => self.constant_instruction("OP_CONSTANT", offset),
            OpCode::GetProperty => self.constant_instruction("OP_GET_PROPERTY", offset),
            OpCode::SetProperty => self.constant_instruction("OP_SET_PROPERTY", offset),
            OpCode::Nil => Self::simple_instruction("OP_NIL", offset),
            OpCode::True => Self::simple_instruction("OP_TRUE", offset),
            OpCode::False => Self::simple_instruction("OP_FALSE", offset),
            OpCode::Pop => Self::simple_instruction("OP_POP", offset),
            OpCode::DefineGlobal => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            OpCode::SetGlobal => self.constant_instruction("OP_SET_GLOBAL", offset),
            OpCode::GetGlobal => self.constant_instruction("OP_GET_GLOBAL", offset),
            OpCode::Class => self.constant_instruction("OP_CLASS", offset),
            OpCode::Method => self.constant_instruction("OP_METHOD", offset),
            OpCode::Negate => Self::simple_instruction("OP_NEGATE", offset),
            OpCode::Print => Self::simple_instruction("OP_PRINT", offset),
            OpCode::Jump => self.jump_instruction("OP_JUMP", 1, offset),
            OpCode::JumpUp => self.jump_instruction("OP_JUMP_UP", -1, offset),
            OpCode::JumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            OpCode::JumpIfTrue => self.jump_instruction("OP_JUMP_IF_TRUE", 1, offset),
            OpCode::Return => Self::simple_instruction("OP_RETURN", offset),
            OpCode::Add => Self::simple_instruction("OP_ADD", offset),
            OpCode::Sub => Self::simple_instruction("OP_SUB", offset),
            OpCode::Mul => Self::simple_instruction("OP_MUL", offset),
            OpCode::Div => Self::simple_instruction("OP_DIV", offset),
            OpCode::Not => Self::simple_instruction("OP_NOT", offset),
            OpCode::Greater => Self::simple_instruction("OP_GREATER", offset),
            OpCode::Less => Self::simple_instruction("OP_LESS", offset),
            OpCode::Equal => Self::simple_instruction("OP_EQUAL", offset),
            OpCode::GetLocal => self.byte_instruction("OP_GET_LOCAL", offset),
            OpCode::SetLocal => self.byte_instruction("OP_SET_LOCAL", offset),
            OpCode::Invoke => self.invoke_instruction("OP_INVOKE", offset),
            OpCode::SuperInvoke => self.invoke_instruction("OP_SUPER_INVOKE", offset),
            OpCode::Closure => {
                offset += 1;
                let constant = self.code[offset];
                offset += 1;
                println!(
                    "{:16} {:4} {}",
                    "OP_CLOSURE", constant, self.constants[constant as usize]
                );

                let function = self.constants[constant as usize].as_obj_function();
                for _ in 0..function.upvalue_count {
                    let is_local = self.code[offset] != 0;
                    offset += 1;
                    let index = self.code[offset];
                    offset += 1;
                    println!(
                        "{:04}      |                     {} {}",
                        offset - 2,
                        if is_local { "local" } else { "upvalue" },
                        index,
                    );
                }

                offset
            }
            OpCode::Inherit => Self::simple_instruction("OP_INHERIT", offset),
            OpCode::CloseUpvalue => Self::simple_instruction("OP_CLOSE_UPVALUE", offset),
            OpCode::GetUpvalue => self.byte_instruction("OP_GET_UPVALUE", offset),
            OpCode::SetUpvalue => self.byte_instruction("OP_SET_UPVALUE", offset),
            OpCode::GetSuper => self.constant_instruction("OP_GET_SUPER", offset),
        }
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:16} {:4} '", name, offset);
        print!("{}", self.constants[constant as usize]);
        println!("'");
        offset + 2
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:16} {:4}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: i8, offset: usize) -> usize {
        let jump = i16::from_ne_bytes([self.code[offset + 1], self.code[offset + 2]]);
        println!(
            "{:16} {:4} -> {}",
            name,
            offset,
            offset as i64 + 3 + sign as i64 * jump as i64
        );
        offset + 3
    }

    fn invoke_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        let arg_count = self.code[offset + 2];
        println!(
            "{:16} ({} args) {:4} {}",
            name, arg_count, constant, self.constants[constant as usize]
        );
        offset + 3
    }
}
