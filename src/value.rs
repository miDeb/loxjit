use std::hash::Hash;
use std::{fmt::Display, hint::unreachable_unchecked};

use crate::object::{NativeFnRef, Obj, ObjContents, ObjFunction};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(*const Obj),
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
        matches!(self, &Value::Obj(o) if matches!(unsafe {&(*o).contents}, ObjContents::String(_)))
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, &Value::Obj(o) if matches!(unsafe {&(*o).contents}, ObjContents::Function(_)))
    }

    pub fn is_native_fun(&self) -> bool {
        matches!(self, &Value::Obj(o) if matches!(unsafe {&(*o).contents}, ObjContents::NativeFunction(_)))
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn as_obj(&self) -> *const Obj {
        match self {
            Value::Obj(o) => *o,
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn as_string(&self) -> &'static str {
        match self {
            &Value::Obj(o) => match unsafe { &(*o).contents } {
                ObjContents::String(s) => s,
                _ => unsafe { unreachable_unchecked() },
            },
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn as_fun(&self) -> &'static ObjFunction {
        match self {
            &Value::Obj(o) => match unsafe { &(*o).contents } {
                ObjContents::Function(fun) => fun,
                _ => unsafe { unreachable_unchecked() },
            },
            _ => unsafe { unreachable_unchecked() },
        }
    }
    pub fn as_native_fun(&self) -> NativeFnRef {
        match self {
            &Value::Obj(o) => match unsafe { &(*o).contents } {
                ObjContents::NativeFunction(fun) => fun.inner,
                _ => unsafe { unreachable_unchecked() },
            },
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl<T> From<T> for Value
where
    Obj: From<T>,
{
    fn from(obj: T) -> Self {
        let obj = obj.into();
        Value::Obj(Box::into_raw(Box::new(obj)))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(bool) => write!(f, "{bool}"),
            Value::Nil => write!(f, "nil"),
            Value::Number(number) => write!(f, "{number}"),
            Value::Obj(o) => write!(f, "{}", unsafe { &**o }),
        }
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        debug_assert!(self.is_string());
        self.as_string().hash(state)
    }
}
