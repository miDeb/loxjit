#![allow(dead_code)]

use std::fmt::Display;
use std::hash::Hash;
use std::mem::MaybeUninit;

use once_cell::sync::Lazy;

use crate::emitter::FnInfo;
use crate::gc::{intern_const_string, register_const_object, GcCell};
use crate::properties::{ObjShape, PropertyList};
use crate::value::Value;

pub static mut INIT_STRING: Lazy<GcCell<ObjString>> =
    Lazy::new(|| intern_const_string("init".into()));

macro_rules! objImpl {
    ($obj_name: ident, $enum_name: ident, $is_fn: ident, $as_fn: ident,$as_mut_fn: ident) => {
        impl ObjHeader {
            pub fn $is_fn(&self) -> bool {
                self.obj_type == ObjType::$enum_name
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
                    obj_type: ObjType::$enum_name,
                }
            }
        }

        impl Value {
            pub fn $is_fn(&self) -> bool {
                self.is_obj() && self.as_obj().obj_type == ObjType::$enum_name
            }

            pub fn $as_fn(&self) -> GcCell<$obj_name> {
                debug_assert_eq!(self.as_obj().obj_type, ObjType::$enum_name);
                unsafe { self.as_obj().cast() }
            }
        }

        impl From<GcCell<$obj_name>> for GcCell<ObjHeader> {
            fn from(obj: GcCell<$obj_name>) -> Self {
                unsafe { obj.cast() }
            }
        }

        impl From<GcCell<$obj_name>> for Value {
            fn from(obj: GcCell<$obj_name>) -> Self {
                Value::from_obj(unsafe { obj.cast() })
            }
        }

        impl From<&GcCell<$obj_name>> for GcCell<ObjHeader> {
            fn from(obj: &GcCell<$obj_name>) -> Self {
                unsafe { obj.cast() }
            }
        }

        impl From<&GcCell<$obj_name>> for Value {
            fn from(obj: &GcCell<$obj_name>) -> Self {
                Value::from_obj(unsafe { obj.cast() })
            }
        }
    };
}

objImpl!(
    ObjString,
    String,
    is_obj_string,
    as_obj_string,
    as_obj_string_mut
);
objImpl!(
    ObjClosure,
    Closure,
    is_obj_closure,
    as_obj_closure,
    as_obj_closure_mut
);
objImpl!(
    ObjFunction,
    Function,
    is_obj_function,
    as_obj_function,
    as_obj_function_mut
);
objImpl!(
    ObjUpvalue,
    Upvalue,
    is_obj_upvalue,
    as_obj_upvalue,
    as_obj_upvalue_mut
);
objImpl!(
    ObjClass,
    Class,
    is_obj_class,
    as_obj_class,
    as_obj_class_mut
);
objImpl!(
    ObjInstance,
    Instance,
    is_obj_instance,
    as_obj_instance,
    as_obj_instance_mut
);
objImpl!(
    ObjBoundMethod,
    BoundMethod,
    is_obj_bound_method,
    as_obj_bound_method,
    as_obj_bound_method_mut
);
objImpl!(
    ObjShape,
    Shape,
    is_obj_shape,
    as_obj_shape,
    as_obj_shape_mut
);

#[derive(PartialEq, Eq, Debug, Clone)]
#[repr(u8)]
pub enum ObjType {
    Function,
    String,
    Closure,
    Upvalue,
    Class,
    Instance,
    BoundMethod,
    Shape,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct ObjHeader {
    pub obj_type: ObjType,
}

impl Display for ObjHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.obj_type {
            ObjType::Function => self.as_obj_function().fmt(f),
            ObjType::String => self.as_obj_string().fmt(f),
            ObjType::Closure => self.as_obj_closure().fmt(f),
            ObjType::Class => self.as_obj_class().fmt(f),
            ObjType::Instance => self.as_obj_instance().fmt(f),
            ObjType::BoundMethod => self.as_obj_bound_method().fmt(f),
            t => panic!("tried to format internal VM value {t:?}"),
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
    pub name: Option<GcCell<ObjString>>,
    pub upvalue_count: usize,
    pub fn_info: Option<FnInfo>,
}

impl ObjFunction {
    pub fn new(arity: u8, name: Option<GcCell<ObjString>>, fn_info: Option<FnInfo>) -> Self {
        Self {
            header: Self::header(),
            arity,
            name,
            upvalue_count: 0,
            fn_info,
        }
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

#[repr(C)]
pub struct ObjClosure {
    header: ObjHeader,

    pub function: *const u8,
    pub upvalues: (*mut GcCell<ObjUpvalue>, usize, usize),
    name: Option<GcCell<ObjString>>,
}

impl ObjClosure {
    pub fn new(
        function: *const u8,
        name: Option<GcCell<ObjString>>,
        upvalues: Vec<GcCell<ObjUpvalue>>,
    ) -> Self {
        Self {
            header: Self::header(),
            function,
            name,
            upvalues: upvalues.into_raw_parts(),
        }
    }
}

impl Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name {
            write!(f, "<fn {name}>")
        } else {
            write!(f, "<native fn>")
        }
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    pub header: ObjHeader,

    pub location: *mut Value,
    pub next: Option<GcCell<ObjUpvalue>>,
    pub closed: MaybeUninit<Value>,
}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

#[repr(C)]
pub struct ObjClass {
    header: ObjHeader,
    pub name: GcCell<ObjString>,
    pub init: Option<GcCell<ObjClosure>>,
    pub methods: Vec<GcCell<ObjClosure>>,
    pub shape: GcCell<ObjShape>,
}

impl ObjClass {
    pub fn new(name: GcCell<ObjString>) -> Self {
        Self {
            header: Self::header(),
            name,
            init: None,
            methods: Default::default(),
            shape: register_const_object(ObjShape::empty()),
        }
    }
}

impl Display for ObjClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[repr(C)]
pub struct ObjInstance {
    header: ObjHeader,
    pub class: GcCell<ObjClass>,
    pub shape: GcCell<ObjShape>,
    pub fields: PropertyList,
}

impl ObjInstance {
    pub fn new(class: GcCell<ObjClass>) -> Self {
        Self {
            header: Self::header(),
            class,
            fields: PropertyList::new(),
            shape: class.shape,
        }
    }
}

impl Display for ObjInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}

#[repr(C)]
pub struct ObjBoundMethod {
    header: ObjHeader,
    pub receiver: Value,
    pub method: GcCell<ObjClosure>,
}

impl ObjBoundMethod {
    pub fn new(receiver: Value, method: GcCell<ObjClosure>) -> Self {
        Self {
            header: Self::header(),
            receiver,
            method,
        }
    }
}

impl Display for ObjBoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.method)
    }
}
