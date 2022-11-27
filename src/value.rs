use std::fmt::Display;

use crate::gc::GcCell;
use crate::object::ObjHeader;

pub const QNAN: u64 = 0x7ffc000000000000;
const TAG_NIL: u64 = 1;
const TAG_FALSE: u64 = 2;
const TAG_TRUE: u64 = 3;
const TAG_UNINIT: u64 = 4;

pub const SIGN_BIT: u64 = 0x8000000000000000;

pub const NIL_VAL: Value = Value(QNAN | TAG_NIL);
pub const FALSE_VAL: Value = Value(QNAN | TAG_FALSE);
pub const TRUE_VAL: Value = Value(QNAN | TAG_TRUE);
pub const UNINIT_VAL: Value = Value(QNAN | TAG_UNINIT);

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Value(u64);

impl Value {
    pub fn from_obj(obj: GcCell<ObjHeader>) -> Self {
        Self(SIGN_BIT | QNAN | obj.to_bits())
    }

    pub fn is_number(self) -> bool {
        (self.0 & QNAN) != QNAN
    }

    pub fn as_number(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub fn is_obj(self) -> bool {
        ((self.0) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)
    }

    pub fn as_obj(self) -> GcCell<ObjHeader> {
        unsafe { GcCell::from_bits((self.0) & !(SIGN_BIT | QNAN)) }
    }

    pub fn to_bits(self) -> u64 {
        self.0
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == FALSE_VAL.0 {
            write!(f, "false")
        } else if self.0 == TRUE_VAL.0 {
            write!(f, "true")
        } else if self.0 == NIL_VAL.0 {
            write!(f, "nil")
        } else if self.is_obj() {
            write!(f, "{}", self.as_obj())
        } else {
            write!(f, "{}", self.as_number())
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.is_number() && other.is_number() {
            self.as_number() == other.as_number()
        } else {
            self.0 == other.0
        }
    }
}
