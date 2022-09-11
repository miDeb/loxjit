#![allow(dead_code)]

use std::fmt::Display;
use std::hash::Hash;
use std::mem::MaybeUninit;

use crate::chunk::Chunk;
use crate::gc::{GcCell, GcFreeHandle, NoTrace, Trace};
use crate::interned_strings::InternedString;
use crate::value::Value;

macro_rules! objImpl {
    ($obj_name: ident, $is_fn: ident, $as_fn: ident,$as_mut_fn: ident) => {
        impl ObjHeader {
            pub fn $is_fn(&self) -> bool {
                self.obj_type == ObjType::$obj_name
            }

            pub fn $as_fn(&self) -> &$obj_name {
                debug_assert!(self.$is_fn());
                unsafe { std::mem::transmute(self) }
            }
            pub fn $as_mut_fn(&mut self) -> &mut $obj_name {
                debug_assert!(self.$is_fn());
                unsafe { std::mem::transmute(self) }
            }
        }

        impl $obj_name {
            pub fn header() -> ObjHeader {
                ObjHeader {
                    obj_type: ObjType::$obj_name,
                }
            }
        }

        impl From<GcCell<$obj_name>> for GcCell<ObjHeader> {
            fn from(obj: GcCell<$obj_name>) -> Self {
                unsafe { obj.cast() }
            }
        }

        impl From<GcCell<$obj_name>> for Value {
            fn from(obj: GcCell<$obj_name>) -> Self {
                Value::Obj(unsafe { obj.cast() })
            }
        }
    };
}

objImpl!(ObjString, is_obj_string, as_obj_string, as_obj_string_mut);
objImpl!(
    ObjClosure,
    is_obj_closure,
    as_obj_closure,
    as_obj_closure_mut
);
objImpl!(
    ObjFunction,
    is_obj_function,
    as_obj_function,
    as_obj_function_mut
);
objImpl!(
    ObjUpvalue,
    is_obj_upvalue,
    as_obj_upvalue,
    as_obj_upvalue_mut
);
objImpl!(NativeFn, is_native_fn, as_native_fn, as_native_fn_mut);

#[derive(PartialEq, Eq, Debug)]
pub enum ObjType {
    ObjFunction,
    NativeFn,
    ObjString,
    ObjClosure,
    ObjUpvalue,
}

#[derive(Debug)]
#[repr(C)]
pub struct ObjHeader {
    pub obj_type: ObjType,
}

impl Trace for ObjHeader {
    fn trace(&self) {
        match self.obj_type {
            ObjType::ObjFunction => self.as_obj_function().trace(),
            ObjType::NativeFn => self.as_native_fn().trace(),
            ObjType::ObjString => self.as_obj_string().trace(),
            ObjType::ObjClosure => self.as_obj_closure().trace(),
            ObjType::ObjUpvalue => self.as_obj_upvalue().trace(),
        }
    }
}

impl Display for ObjHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.obj_type {
            ObjType::ObjFunction => self.as_obj_function().fmt(f),
            ObjType::NativeFn => self.as_native_fn().fmt(f),
            ObjType::ObjString => self.as_obj_string().fmt(f),
            ObjType::ObjClosure => self.as_obj_closure().fmt(f),
            ObjType::ObjUpvalue => self.as_obj_upvalue().fmt(f),
        }
    }
}

impl PartialEq for ObjHeader {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for ObjHeader {}

#[repr(C)]
pub struct ObjString {
    header: ObjHeader,
    pub string: String,
}

impl ObjString {
    pub fn new(string: String) -> Self {
        Self {
            header: Self::header(),
            string,
        }
    }
}

impl InternedString for ObjString {}

impl NoTrace for ObjString {}

impl Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl Hash for ObjString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for ObjString {}

#[repr(C)]
pub struct ObjFunction {
    header: ObjHeader,

    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<Box<str>>,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn new(arity: u8, chunk: Chunk, name: Option<Box<str>>) -> Self {
        Self {
            header: Self::header(),
            arity,
            chunk,
            name,
            upvalue_count: 0,
        }
    }
}

impl Trace for ObjFunction {
    fn trace(&self) {
        self.chunk.trace()
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "<fn {}>", name)
        } else {
            write!(f, "<script>")
        }
    }
}

pub type NativeFnRef = fn(&[Value]) -> Value;

#[repr(C)]
pub struct NativeFn {
    header: ObjHeader,

    pub inner: NativeFnRef,
}

impl NativeFn {
    pub fn new(inner: NativeFnRef) -> Self {
        Self {
            header: Self::header(),
            inner,
        }
    }
}

impl NoTrace for NativeFn {}

impl Display for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

#[repr(C)]
pub struct ObjClosure {
    header: ObjHeader,

    pub function: GcCell<ObjFunction>,
    pub upvalues: Box<[Option<GcCell<ObjUpvalue>>]>,
}

impl From<GcCell<ObjFunction>> for ObjClosure {
    fn from(fun: GcCell<ObjFunction>) -> Self {
        Self {
            header: Self::header(),
            upvalues: vec![None; fun.borrow().upvalue_count].into_boxed_slice(),
            function: fun,
        }
    }
}

impl Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self.function.borrow())
    }
}

impl Trace for ObjClosure {
    fn trace(&self) {
        self.function.trace();
        self.upvalues.trace();
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    pub header: ObjHeader,

    pub location: *mut Value,
    pub next: Option<GcCell<ObjUpvalue>>,
    pub closed: MaybeUninit<Value>,
}

impl Trace for ObjUpvalue {
    fn trace(&self) {
        unsafe { &*self.location }.trace()
    }
}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

pub fn free_obj(handle: GcFreeHandle<ObjString>) {
    unsafe {
        match handle.get::<ObjHeader>().obj_type {
            ObjType::ObjFunction => handle.free::<ObjFunction>(),
            ObjType::NativeFn => handle.free::<NativeFn>(),
            ObjType::ObjString => handle.free_interned_string(),
            ObjType::ObjClosure => handle.free::<ObjClosure>(),
            ObjType::ObjUpvalue => handle.free::<ObjUpvalue>(),
        }
    }
}
