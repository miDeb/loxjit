use std::fmt::Display;
use std::hash::Hash;
use std::mem::MaybeUninit;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(PartialEq, Eq, Hash)]
pub struct Obj {
    pub contents: ObjContents,
}

impl From<Box<ObjFunction>> for Obj {
    fn from(fun: Box<ObjFunction>) -> Self {
        Self {
            contents: ObjContents::Function(*fun),
        }
    }
}

impl From<Box<ObjClosure>> for Obj {
    fn from(closure: Box<ObjClosure>) -> Self {
        Self {
            contents: ObjContents::ObjClosure(*closure),
        }
    }
}

impl From<NativeFnRef> for Obj {
    fn from(fun: NativeFnRef) -> Self {
        Self {
            contents: ObjContents::NativeFunction(NativeFn::new(fun)),
        }
    }
}

impl From<ObjUpvalue> for Obj {
    fn from(upvalue: ObjUpvalue) -> Self {
        Self {
            contents: ObjContents::ObjUpvalue(upvalue)
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum ObjContents {
    String(Box<str>),
    Function(ObjFunction),
    NativeFunction(NativeFn),
    ObjClosure(ObjClosure),
    ObjUpvalue(ObjUpvalue),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.contents {
            ObjContents::String(s) => write!(f, "{}", &*s),
            ObjContents::Function(fun) => {
                if let Some(name) = &fun.name {
                    write!(f, "<fn {}>", name)
                } else {
                    write!(f, "<script>")
                }
            }
            ObjContents::NativeFunction(_) => write!(f, "<native fn>"),
            ObjContents::ObjClosure(closure) => {
                if let Some(name) = &unsafe { &*closure.function }.name {
                    write!(f, "<fn {}>", name)
                } else {
                    write!(f, "<script>")
                }
            }
            ObjContents::ObjUpvalue(_) => write!(f, "upvalue"),
        }
    }
}

impl From<String> for Obj {
    fn from(s: String) -> Self {
        Self {
            contents: ObjContents::String(s.into_boxed_str()),
        }
    }
}

pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<Box<str>>,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn new(arity: u8, chunk: Chunk, name: Option<Box<str>>) -> Self {
        Self {
            arity,
            chunk,
            name,
            upvalue_count: 0,
        }
    }
}

// Only the string interner needs to hash and compare objects, so it should never reach this part:

impl Hash for ObjFunction {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        unreachable!()
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!()
    }
}

impl Eq for ObjFunction {}

pub type NativeFnRef = fn(&[Value]) -> Value;

pub struct NativeFn {
    pub inner: NativeFnRef,
}

impl NativeFn {
    fn new(inner: NativeFnRef) -> Self {
        Self { inner }
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!()
    }
}

impl Eq for NativeFn {}
impl Hash for NativeFn {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        unreachable!()
    }
}

pub struct ObjClosure {
    function: *const ObjFunction,
    pub upvalues: Box<[Option<*const ObjUpvalue>]>,
}

impl ObjClosure {
    pub fn as_function(&self) -> &ObjFunction {
        unsafe { &*self.function }
    }
}

impl From<&ObjFunction> for ObjClosure {
    fn from(function: &ObjFunction) -> Self {
        let upvalues = vec![None; function.upvalue_count].into_boxed_slice();
        Self { function, upvalues }
    }
}

impl PartialEq for ObjClosure {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!()
    }
}

impl Eq for ObjClosure {}
impl Hash for ObjClosure {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        unreachable!()
    }
}

pub struct ObjUpvalue {
    pub location: *mut Value,
    pub next: Option<*mut ObjUpvalue>,
    pub closed: MaybeUninit<Value>,
}

impl PartialEq for ObjUpvalue {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!()
    }
}

impl Eq for ObjUpvalue {}
impl Hash for ObjUpvalue {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        unreachable!()
    }
}
