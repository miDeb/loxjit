use std::fmt::Display;
use std::hash::Hash;

use crate::chunk::Chunk;
use crate::value::Value;

#[derive(PartialEq, Eq, Hash)]
pub struct Obj {
    pub contents: ObjContents,
}

impl Obj {
    pub fn new_function(arity: u8, chunk: Chunk, name: Option<Box<str>>) -> Self {
        Self {
            contents: ObjContents::Function(ObjFunction::new(arity, chunk, name)),
        }
    }
}

impl From<Box<ObjFunction>> for Obj {
    fn from(fun: Box<ObjFunction>) -> Self {
        Self {
            contents: ObjContents::Function(*fun),
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

#[derive(PartialEq, Eq, Hash)]
pub enum ObjContents {
    String(Box<str>),
    Function(ObjFunction),
    NativeFunction(NativeFn),
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
}

impl ObjFunction {
    pub fn new(arity: u8, chunk: Chunk, name: Option<Box<str>>) -> Self {
        Self { arity, chunk, name }
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
    fn new(inner: NativeFnRef) -> Self { Self { inner } }
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
