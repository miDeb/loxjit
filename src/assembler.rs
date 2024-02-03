use memmap2::{Mmap, MmapMut};

use crate::value::Value;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct AssemblyOffset(pub usize);

#[derive(Clone, Copy)]
pub enum Operand {
    Register(Register),
    Immediate(u64),
    Memory(Register, i32),
}

#[derive(Clone, Copy)]
pub enum FloatOperand {
    Register(FloatRegister),
    Immediate(f64),
    Memory(Register, i32),
}

trait IntoRegisterIndex {
    fn upper_bit(&self) -> u8;
    fn base_reg(&self) -> u8;
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum Register {
    Rax = 0,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum FloatRegister {
    Xmm0 = 0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

impl IntoRegisterIndex for Register {
    fn base_reg(&self) -> u8 {
        *self as u8 & 0b111
    }

    fn upper_bit(&self) -> u8 {
        (*self as u8 & 0b1000) >> 3
    }
}

impl IntoRegisterIndex for FloatRegister {
    fn base_reg(&self) -> u8 {
        *self as u8 & 0b111
    }

    fn upper_bit(&self) -> u8 {
        (*self as u8 & 0b1000) >> 3
    }
}

impl IntoRegisterIndex for u8 {
    fn base_reg(&self) -> u8 {
        assert!(self < &16, "Invalid register index");
        *self & 0b111
    }

    fn upper_bit(&self) -> u8 {
        assert!(self < &16, "Invalid register index");
        (*self & 0b1000) >> 3
    }
}

enum Mod {
    Direct,
    Indirect,
    IndirectDisplacement,
}

impl Mod {
    fn to_bits(&self) -> u8 {
        match self {
            Mod::Direct => 0b11,
            Mod::Indirect => 0b00,
            Mod::IndirectDisplacement => 0b10,
        }
    }

    fn is_indirect(&self) -> bool {
        matches!(self, Mod::Indirect | Mod::IndirectDisplacement)
    }
}

pub struct Assembler {
    ops: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Self { ops: Vec::new() }
    }

    pub fn get_current_offset(&self) -> AssemblyOffset {
        AssemblyOffset(self.ops.len())
    }

    pub fn make_executable(&mut self) -> Mmap {
        let mut mmap = MmapMut::map_anon(self.ops.len()).unwrap();

        mmap.copy_from_slice(&self.ops);
        mmap.flush().unwrap();
        mmap.make_exec().unwrap()
    }

    pub fn append(&mut self, value: u8) {
        self.ops.push(value)
    }

    pub fn append_u32(&mut self, value: u32) {
        self.ops.extend_from_slice(&value.to_le_bytes())
    }

    pub fn append_i32(&mut self, value: i32) {
        self.ops.extend_from_slice(&value.to_le_bytes())
    }

    pub fn append_u64(&mut self, value: u64) {
        self.ops.extend_from_slice(&value.to_le_bytes())
    }

    fn append_rexw_for_modrm(&mut self, reg: impl IntoRegisterIndex, rm: impl IntoRegisterIndex) {
        self.append(0x48 | reg.upper_bit() << 2 | rm.upper_bit());
    }

    fn maybe_append_rex_for_modrm(
        &mut self,
        reg: impl IntoRegisterIndex,
        rm: impl IntoRegisterIndex,
    ) {
        if reg.upper_bit() != 0 || rm.upper_bit() != 0 {
            self.append(0x40 | reg.upper_bit() << 2 | rm.upper_bit());
        }
    }

    fn append_rexw_for_reg(&mut self, reg: Register) {
        self.append(0x48 | reg.upper_bit())
    }

    fn maybe_append_rex(&mut self, reg: Register) {
        if reg.upper_bit() != 0 {
            self.append(0x40 | reg.upper_bit())
        }
    }

    fn append_modrm(&mut self, m: Mod, reg: impl IntoRegisterIndex, rm: impl IntoRegisterIndex) {
        self.append((m.to_bits() << 6) + (reg.base_reg() << 3) + rm.base_reg());
        if rm.base_reg() == 0b100 && m.is_indirect() {
            self.append(0x24);
        }
    }

    pub fn push(&mut self, src: Operand) {
        match src {
            // push reg
            Operand::Register(src) => {
                self.maybe_append_rex(src);
                self.append(0x50 + src.base_reg());
            }
            // push memory
            Operand::Memory(src, offset) => {
                self.maybe_append_rex(src);
                self.append(0xff);
                self.append_modrm(Mod::IndirectDisplacement, 6, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn pop(&mut self, dest: Operand) {
        match dest {
            // pop reg
            Operand::Register(dest) => {
                self.maybe_append_rex(dest);
                self.append(0x58 + dest.base_reg());
            }
            // pop memory
            Operand::Memory(dest, offset) => {
                self.maybe_append_rex(dest);
                self.append(0x8f);
                self.append_modrm(Mod::IndirectDisplacement, 0, dest);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn mov(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // mov reg, imm64
            (Operand::Register(dest), Operand::Immediate(src)) => {
                self.append_rexw_for_reg(dest);
                self.append(0xb8 + dest.base_reg());
                self.append_u64(src);
            }
            // mov memory, reg
            (Operand::Memory(dest, offset), Operand::Register(src)) => {
                self.append_rexw_for_modrm(src, dest);
                self.append(0x89);
                self.append_modrm(Mod::IndirectDisplacement, src, dest);
                self.append_i32(offset);
            }
            // mov reg, reg
            (Operand::Register(dest), Operand::Register(src)) => {
                self.append_rexw_for_modrm(src, dest);
                self.append(0x89);
                self.append_modrm(Mod::Direct, src, dest);
            }
            // mov reg, memory
            (Operand::Register(dest), Operand::Memory(src, offset)) => {
                self.append_rexw_for_modrm(dest, src);
                self.append(0x8b);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn movsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // movsd memory, reg
            (FloatOperand::Memory(dest, offset), FloatOperand::Register(src)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(src, dest);
                self.append(0x0f);
                self.append(0x11);
                self.append_modrm(Mod::IndirectDisplacement, src, dest);
                self.append_i32(offset);
            }

            // movsd reg, memory
            (FloatOperand::Register(dest), FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x10);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // add reg, imm8
            (Operand::Register(dest), Operand::Immediate(src)) if src <= u8::MAX as u64 => {
                self.append_rexw_for_reg(dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 0, dest);
                self.append(src as u8);
            }
            _ => unimplemented!(),
        }
    }

    pub fn addsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // addsd reg, memory
            (FloatOperand::Register(dest), FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x58);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn subsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // subsd reg, memory
            (FloatOperand::Register(dest), FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x5c);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn mulsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // mulsd reg, memory
            (FloatOperand::Register(dest), FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x59);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn divsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // divsd reg, memory
            (FloatOperand::Register(dest), FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x5e);
                self.append_modrm(Mod::IndirectDisplacement, dest, src);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn cmp(&mut self, left: Operand, right: Operand) {
        match (left, right) {
            // cmp reg, memory
            (Operand::Register(left), Operand::Memory(right, offset)) => {
                self.append_rexw_for_modrm(left, right);
                self.append(0x3b);
                self.append_modrm(Mod::IndirectDisplacement, left, right);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn call(&mut self, dest: Operand, tmp_register: Register) {
        // call reg
        self.align_stack_before_call(tmp_register);
        match dest {
            Operand::Register(dest) => {
                self.maybe_append_rex(dest);
                self.append(0xff);
                self.append(0xd0 + dest.base_reg());
            }
            _ => unimplemented!(),
        }
        self.restore_stack_after_call();
    }

    fn align_stack_before_call(&mut self, tmp_register: Register) {
        self.mov(
            Operand::Register(tmp_register),
            Operand::Register(Register::Rsp),
        );
        self.or(Operand::Register(Register::Rsp), Operand::Immediate(0x08));
        self.push(
            Operand::Register(tmp_register),
        );
    }

    fn restore_stack_after_call(&mut self) {
        self.pop(Operand::Register(Register::Rsp));
    }

    pub fn or(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // or register, imm8
            (Operand::Register(dest), Operand::Immediate(src)) if src <= u8::MAX as u64 => {
                self.append_rexw_for_modrm(1, dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 1, dest);
                self.append(src as u8);
            }
            _ => unimplemented!(),
        }
    }

    pub fn ret(&mut self) {
        self.append(0xc3);
    }
}
