
use std::fmt::Display;

use crate::gc::{GcCell, Trace};
use crate::object::NativeFnRef;
use crate::object::{NativeFn, ObjClosure, ObjFunction, ObjHeader, ObjString, ObjType};

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

    pub fn is_string(&self) -> bool {
        matches!(self, &Value::Obj(o) if o.borrow().obj_type == ObjType::ObjString)
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, &Value::Obj(o) if o.borrow().obj_type == ObjType::ObjFunction)
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, &Value::Obj(o) if o.borrow().obj_type == ObjType::ObjClosure)
    }

    pub fn is_native_fun(&self) -> bool {
        matches!(self, &Value::Obj(o) if o.borrow().obj_type == ObjType::NativeFn)
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

    pub fn as_string(&self) -> GcCell<ObjString> {
        match self {
            &Value::Obj(o) => {
                assert!(self.is_string());
                unsafe { o.cast() }
            }
            _ => unreachable!(),
        }
    }

    pub fn as_fun(&self) -> GcCell<ObjFunction> {
        match self {
            &Value::Obj(o) => {
                assert!(self.is_fun());
                unsafe { o.cast() }
            }
            _ => unreachable!(),
        }
    }

    pub fn as_closure(&self) -> GcCell<ObjClosure> {
        match self {
            &Value::Obj(o) => {
                assert!(self.is_closure());
                unsafe { o.cast() }
            }
            _ => unreachable!(),
        }
    }
    pub fn as_native_fun(&self) -> NativeFnRef {
        match self {
            &Value::Obj(o) => {
                assert!(self.is_native_fun());
                unsafe { o.cast::<NativeFn>() }.borrow().inner
            }
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
