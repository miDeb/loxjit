use num_enum::{IntoPrimitive, TryFromPrimitive, UnsafeFromPrimitive};

use crate::value::{print_value, Value};

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, IntoPrimitive, UnsafeFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Sub,
    Mul,
    Div,
    Not,
    Negate,
    Return,
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

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let op_code: OpCode = self.code[offset].try_into().unwrap();

        match op_code {
            OpCode::Constant => Self::constant_instruction(&self, "OP_CONSTANT", offset),
            OpCode::Nil => Self::simple_instruction("OP_NIL", offset),
            OpCode::True => Self::simple_instruction("OP_TRUE", offset),
            OpCode::False => Self::simple_instruction("OP_FALSE", offset),
            OpCode::Negate => Self::simple_instruction("OP_NEGATE", offset),
            OpCode::Return => Self::simple_instruction("OP_RETURN", offset),
            OpCode::Add => Self::simple_instruction("OP_ADD", offset),
            OpCode::Sub => Self::simple_instruction("OP_SUB", offset),
            OpCode::Mul => Self::simple_instruction("OP_MUL", offset),
            OpCode::Div => Self::simple_instruction("OP_DIV", offset),
            OpCode::Not => Self::simple_instruction("OP_NOT", offset),
            OpCode::Greater => Self::simple_instruction("OP_GREATER", offset),
            OpCode::Less => Self::simple_instruction("OP_LESS", offset),
            OpCode::Equal => Self::simple_instruction("OP_EQUAL", offset),
        }
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:16} {:4} '", name, offset);
        print_value(self.constants[constant as usize]);
        println!("'");
        offset + 2
    }
}
