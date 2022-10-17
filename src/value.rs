use std::fmt::Display;

use crate::gc::{GcCell, Trace};
use crate::object::ObjHeader;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(GcCell<ObjHeader>),
}

impl Value {
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_obj(&self) -> bool {
        matches!(self, Value::Obj(_))
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            _ => unreachable!(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => unreachable!(),
        }
    }

    pub fn as_obj(&self) -> GcCell<ObjHeader> {
        match self {
            Value::Obj(o) => *o,
            _ => unreachable!(),
        }
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl Trace for Value {
    fn trace(&self) {
        match self {
            Value::Bool(_) => {}
            Value::Nil => {}
            Value::Number(_) => {}
            Value::Obj(obj) => obj.trace(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(bool) => write!(f, "{bool}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(number) => write!(f, "{number}"),
            Value::Obj(o) => write!(f, "{}", o),
        }
    }
}
