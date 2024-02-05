use memmap2::{Mmap, MmapMut};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct AssemblyOffset(pub usize);

pub struct LabelUse {
    patch_offset: AssemblyOffset,
    displacement_relative_to: AssemblyOffset,
}

pub enum Label {
    WithTarget(AssemblyOffset),
    WithUses(Vec<LabelUse>),
}

impl Label {
    fn new() -> Self {
        Self::WithUses(Vec::new())
    }
}

#[derive(Clone, Copy)]
pub enum Operand {
    Register(Register),
    Imm64(u64),
    Imm32(u32),
    Imm8(u8),
    Memory(Register, i32),
}

#[derive(Clone, Copy)]
pub enum FloatOperand {
    Register(FloatRegister),
    Immediate(f64),
    Memory(Register, i32),
}

impl From<FloatOperand> for Operand {
    fn from(operand: FloatOperand) -> Self {
        match operand {
            FloatOperand::Register(reg) => Operand::Register(reg.into()),
            FloatOperand::Immediate(imm) => Operand::Imm64(imm.to_bits()),
            FloatOperand::Memory(reg, offset) => Operand::Memory(reg, offset),
        }
    }
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

impl From<FloatRegister> for Register {
    fn from(reg: FloatRegister) -> Self {
        unsafe { std::mem::transmute(reg as u8) }
    }
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

    pub fn create_label(&mut self) -> Label {
        Label::new()
    }

    pub fn bind_label(&mut self, label: &mut Label) {
        if let Label::WithUses(uses) = label {
            for label_use in uses.iter() {
                let displacement = self.get_current_offset().0 as i32
                    - label_use.displacement_relative_to.0 as i32;
                let patch_offset = label_use.patch_offset.0;
                self.ops[patch_offset..patch_offset + 4]
                    .copy_from_slice(&displacement.to_le_bytes());
            }
            *label = Label::WithTarget(self.get_current_offset());
        } else {
            panic!("Label already bound");
        }
    }

    fn use_label(&mut self, label: &mut Label, label_use: LabelUse) {
        match label {
            Label::WithUses(uses) => {
                uses.push(label_use);
            }
            Label::WithTarget(target) => {
                let displacement = target.0 as i32 - label_use.displacement_relative_to.0 as i32;
                let patch_offset = label_use.patch_offset.0;
                self.ops[patch_offset..patch_offset + 4]
                    .copy_from_slice(&displacement.to_le_bytes());
            }
        }
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

    fn append_modrm_with_offset(&mut self, reg: impl IntoRegisterIndex, rm: Operand) {
        match rm {
            Operand::Register(rm) => {
                self.append_modrm(Mod::Direct, reg, rm);
            }
            Operand::Memory(rm, 0) => {
                self.append_modrm(Mod::Indirect, reg, rm);
            }
            Operand::Memory(rm, offset) => {
                self.append_modrm(Mod::IndirectDisplacement, reg, rm);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
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
            rm @ Operand::Memory(src, _) => {
                self.maybe_append_rex(src);
                self.append(0xff);
                self.append_modrm_with_offset(6, rm);
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
            rm @ Operand::Memory(dest, _) => {
                self.maybe_append_rex(dest);
                self.append(0x8f);
                self.append_modrm_with_offset(0, rm);
            }
            _ => unimplemented!(),
        }
    }

    pub fn mov(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // mov reg, imm64
            (Operand::Register(dest), Operand::Imm64(src)) => {
                self.append_rexw_for_reg(dest);
                self.append(0xb8 + dest.base_reg());
                self.append_u64(src);
            }
            // mov r/m, reg
            (rm @ Operand::Memory(dest, _), Operand::Register(src))
            | (rm @ Operand::Register(dest), Operand::Register(src)) => {
                self.append_rexw_for_modrm(src, dest);
                self.append(0x89);
                self.append_modrm_with_offset(src, rm)
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

    pub fn cmove(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // cmove reg, reg
            (Operand::Register(dest), Operand::Register(src)) => {
                self.append_rexw_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x44);
                self.append_modrm(Mod::Direct, dest, src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn movsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest.into(), src.into()) {
            // movsd memory, reg
            (rm @ Operand::Memory(dest, _), Operand::Register(src)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(src, dest);
                self.append(0x0f);
                self.append(0x11);
                self.append_modrm_with_offset(src, rm);
            }

            // movsd reg, memory
            (Operand::Register(dest), rm @ Operand::Memory(src, _)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x10);
                self.append_modrm_with_offset(dest, rm)
            }
            _ => unimplemented!(),
        }
    }

    pub fn movq_float_dest(&mut self, dest: FloatOperand, src: Operand) {
        match (dest, src) {
            // movq reg, reg
            (FloatOperand::Register(dest), Operand::Register(src)) => {
                self.append(0x66);
                self.append_rexw_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x6e);
                self.append_modrm(Mod::Direct, dest, src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn add(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // add reg, imm8
            (Operand::Register(dest), Operand::Imm8(src)) => {
                self.append_rexw_for_reg(dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 0, dest);
                self.append(src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn sub(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // sub reg, imm8
            (Operand::Register(dest), Operand::Imm8(src)) => {
                self.append_rexw_for_reg(dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 5, dest);
                self.append(src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn addsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest.into(), src.into()) {
            // addsd reg, r/m
            (Operand::Register(dest), rm @ Operand::Memory(src, _))
            | (Operand::Register(dest), rm @ Operand::Register(src)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x58);
                self.append_modrm_with_offset(dest, rm)
            }
            _ => unimplemented!(),
        }
    }

    pub fn subsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // subsd reg, memory
            (FloatOperand::Register(dest), rm @ FloatOperand::Memory(src, offset)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x5c);
                self.append_modrm_with_offset(dest, rm.into())
            }
            _ => unimplemented!(),
        }
    }

    pub fn mulsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // mulsd reg, memory
            (FloatOperand::Register(dest), rm @ FloatOperand::Memory(src, _)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x59);
                self.append_modrm_with_offset(dest, rm.into())
            }
            _ => unimplemented!(),
        }
    }

    pub fn divsd(&mut self, dest: FloatOperand, src: FloatOperand) {
        match (dest, src) {
            // divsd reg, memory
            (FloatOperand::Register(dest), rm @ FloatOperand::Memory(src, _)) => {
                self.append(0xf2);
                self.maybe_append_rex_for_modrm(dest, src);
                self.append(0x0f);
                self.append(0x5e);
                self.append_modrm_with_offset(dest, rm.into())
            }
            _ => unimplemented!(),
        }
    }

    pub fn cmp(&mut self, left: Operand, right: Operand) {
        match (left, right) {
            // cmp reg, r/m
            (Operand::Register(left), rm @ Operand::Memory(right, _))
            | (Operand::Register(left), rm @ Operand::Register(right)) => {
                self.append_rexw_for_modrm(left, right);
                self.append(0x3b);
                self.append_modrm_with_offset(left, rm)
            }
            _ => unimplemented!(),
        }
    }

    pub fn test(&mut self, left: Operand, right: Operand) {
        match (left, right) {
            // test reg, reg
            (Operand::Register(left), Operand::Register(right)) => {
                self.append_rexw_for_modrm(left, right);
                self.append(0x85);
                self.append_modrm(Mod::Direct, left, right);
            }
            _ => unimplemented!(),
        }
    }

    pub fn jmp(&mut self, label: &mut Label) {
        // jmp rel32
        self.append(0xe9);
        let patch_offset = self.get_current_offset();
        self.append_i32(0);
        let displacement_relative_to = self.get_current_offset();
        self.use_label(
            label,
            LabelUse {
                patch_offset,
                displacement_relative_to,
            },
        );
    }

    pub fn jne(&mut self, label: &mut Label) {
        // jne rel32
        self.append(0x0f);
        self.append(0x85);
        let patch_offset = self.get_current_offset();
        self.append_i32(0);
        let displacement_relative_to = self.get_current_offset();
        self.use_label(
            label,
            LabelUse {
                patch_offset,
                displacement_relative_to,
            },
        );
    }

    pub fn je(&mut self, label: &mut Label) {
        // je rel32
        self.append(0x0f);
        self.append(0x84);
        let patch_offset = self.get_current_offset();
        self.append_i32(0);
        let displacement_relative_to = self.get_current_offset();
        self.use_label(
            label,
            LabelUse {
                patch_offset,
                displacement_relative_to,
            },
        );
    }

    pub fn call_label(&mut self, label: &mut Label) {
        // call rel32
        self.append(0xe8);
        let patch_offset = self.get_current_offset();
        self.append_i32(0);
        let displacement_relative_to = self.get_current_offset();
        self.use_label(
            label,
            LabelUse {
                patch_offset,
                displacement_relative_to,
            },
        );
    }

    pub fn call_extern(&mut self, dest: Operand, tmp_register: Register) {
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
        self.sub(Operand::Register(Register::Rsp), Operand::Imm8(8));
        self.and(Operand::Register(Register::Rsp), Operand::Imm8(0xf0));
        self.mov(
            Operand::Memory(Register::Rsp, 0),
            Operand::Register(tmp_register),
        );
    }

    fn restore_stack_after_call(&mut self) {
        self.pop(Operand::Register(Register::Rsp));
    }

    pub fn xor(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // xor memory, reg
            (Operand::Memory(dest, offset), Operand::Register(src)) => {
                self.append_rexw_for_modrm(src, dest);
                self.append(0x31);
                self.append_modrm(Mod::IndirectDisplacement, src, dest);
                self.append_i32(offset);
            }
            _ => unimplemented!(),
        }
    }

    pub fn or(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // or register, imm8
            (Operand::Register(dest), Operand::Imm8(src)) => {
                self.append_rexw_for_modrm(1, dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 1, dest);
                self.append(src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn and(&mut self, dest: Operand, src: Operand) {
        match (dest, src) {
            // and reg, reg
            (Operand::Register(dest), Operand::Register(src)) => {
                self.append_rexw_for_modrm(src, dest);
                self.append(0x21);
                self.append_modrm(Mod::Direct, src, dest);
            }
            // and reg, imm8
            (Operand::Register(dest), Operand::Imm8(src)) => {
                self.append_rexw_for_modrm(4, dest);
                self.append(0x83);
                self.append_modrm(Mod::Direct, 4, dest);
                self.append(src);
            }
            _ => unimplemented!(),
        }
    }

    pub fn ret(&mut self) {
        self.append(0xc3);
    }
}
