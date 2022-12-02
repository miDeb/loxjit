use std::ptr::NonNull;

use rustc_hash::FxHashMap;

use crate::{
    gc::{register_const_object, GcCell},
    object::{ObjClosure, ObjHeader, ObjString},
    value::Value,
};

#[derive(Clone, Copy)]
pub enum ShapeEntry {
    Present {
        offset: usize,
    },
    Method {
        closure: GcCell<ObjClosure>,
    },
    MissingWithKnownShape {
        shape: GcCell<ObjShape>,
        method_closure: Option<GcCell<ObjClosure>>,
    },
}

#[repr(C)]
#[derive(Clone)]
pub struct ObjShape {
    header: ObjHeader,
    pub entries: FxHashMap<NonNull<ObjString>, ShapeEntry>,
}

impl ObjShape {
    pub fn empty() -> Self {
        Self::new(None)
    }

    fn new(parent: Option<GcCell<ObjShape>>) -> Self {
        Self {
            header: Self::header(),
            entries: if let Some(parent) = parent {
                parent
                    .entries
                    .iter()
                    .filter_map(|(&name, &entry)| match entry {
                        ShapeEntry::MissingWithKnownShape {
                            method_closure: Some(method_closure),
                            ..
                        } => Some((
                            name,
                            ShapeEntry::Method {
                                closure: method_closure,
                            },
                        )),
                        ShapeEntry::MissingWithKnownShape {
                            method_closure: None,
                            ..
                        } => None,
                        e => Some((name, e)),
                    })
                    .collect()
            } else {
                FxHashMap::default()
            },
        }
    }

    pub fn resolve_get_property(
        shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
    ) -> Option<ShapeEntry> {
        match shape.entries.get(&name.as_non_null()) {
            Some(ShapeEntry::MissingWithKnownShape {
                method_closure: Some(offset),
                ..
            }) => Some(ShapeEntry::Method { closure: *offset }),
            Some(e @ ShapeEntry::Method { .. }) | Some(e @ ShapeEntry::Present { .. }) => Some(*e),
            _ => None,
        }
    }

    pub fn add_method(
        mut shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
        closure: GcCell<ObjClosure>,
    ) {
        shape
            .entries
            .insert(name.as_non_null(), ShapeEntry::Method { closure });
    }

    pub fn resolve_set_property(
        mut shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
        property_len: usize,
    ) -> ShapeEntry {
        match shape.entries.get(&name.as_non_null()) {
            e @ None | e @ Some(ShapeEntry::Method { .. }) => {
                let mut new_shape = register_const_object(ObjShape::new(Some(shape)));
                new_shape.entries.insert(
                    name.as_non_null(),
                    ShapeEntry::Present {
                        offset: property_len,
                    },
                );
                let entry = ShapeEntry::MissingWithKnownShape {
                    shape: new_shape,
                    method_closure: if let Some(ShapeEntry::Method { closure }) = e {
                        Some(*closure)
                    } else {
                        None
                    },
                };
                shape.entries.insert(name.as_non_null(), entry);
                entry
            }
            Some(e) => *e,
        }
    }
}

#[repr(transparent)]
pub struct PropertyList {
    properties: (*mut Value, usize, usize),
}

impl PropertyList {
    pub fn new() -> Self {
        Self {
            properties: Vec::new().into_raw_parts(),
        }
    }

    unsafe fn as_vec(&self) -> Vec<Value> {
        unsafe { Vec::from_raw_parts(self.properties.0, self.properties.1, self.properties.2) }
    }

    pub fn get_mut(&mut self, offset: usize) -> &mut Value {
        unsafe { self.properties.0.add(offset).as_mut().unwrap() }
    }

    pub fn get(&self, offset: usize) -> &Value {
        unsafe { self.properties.0.add(offset).as_ref().unwrap() }
    }

    pub fn push(&mut self, val: Value) {
        let mut vec = unsafe { self.as_vec() };
        vec.push(val);
        self.properties = vec.into_raw_parts();
    }

    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.properties.0, self.properties.1) }
    }

    pub fn len(&self) -> usize {
        self.properties.1
    }
}
