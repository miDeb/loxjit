use std::ops::Range;

use rustc_hash::FxHashMap;

use crate::{
    gc::{register_object, GcCell},
    object::ObjString,
    value::Value,
};

#[derive(Clone, Copy)]
pub enum ShapeEntry {
    Present {
        offset: usize,
    },
    Method {
        offset: usize,
    },
    MissingWithKnownShape {
        shape: GcCell<ObjShape>,
        method_offset: Option<usize>,
    },
}

pub struct ObjShape {
    pub entries: FxHashMap<GcCell<ObjString>, ShapeEntry>,
    pub parent: Option<(GcCell<ObjShape>, GcCell<ObjString>)>,
}

impl ObjShape {
    pub fn empty() -> Self {
        Self::new(None)
    }

    pub fn dispose(&mut self) {
        for v in self.entries.values_mut() {
            if let ShapeEntry::MissingWithKnownShape { shape, .. } = v {
                shape.parent = None;
            }
        }
        if let Some((parent_shape, parent_key)) = &mut self.parent {
            parent_shape.entries.remove(parent_key);
        }
    }

    fn new(parent: Option<(GcCell<ObjShape>, GcCell<ObjString>)>) -> Self {
        Self {
            parent,
            entries: if let Some(parent) = parent {
                parent.0.entries.clone()
            } else {
                FxHashMap::default()
            },
        }
    }

    pub fn resolve_get_property(
        shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
    ) -> Option<ShapeEntry> {
        match shape.entries.get(&name) {
            Some(ShapeEntry::MissingWithKnownShape {
                method_offset: Some(offset),
                ..
            }) => Some(ShapeEntry::Method { offset: *offset }),
            Some(e @ ShapeEntry::Method { .. }) | Some(e @ ShapeEntry::Present { .. }) => Some(*e),
            _ => None,
        }
    }

    pub fn add_method(mut shape: GcCell<ObjShape>, name: GcCell<ObjString>, method_len: usize) {
        shape
            .entries
            .insert(name, ShapeEntry::Method { offset: method_len });
    }

    pub fn resolve_set_property(
        stack: Range<*const Value>,
        mut shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
        property_len: usize,
    ) -> ShapeEntry {
        match shape.entries.get(&name) {
            e @ None | e @ Some(ShapeEntry::Method { .. }) => {
                let mut new_shape = register_object(ObjShape::new(Some((shape, name))), stack);
                new_shape.entries.insert(
                    name,
                    ShapeEntry::Present {
                        offset: property_len,
                    },
                );
                let entry = ShapeEntry::MissingWithKnownShape {
                    shape: new_shape,
                    method_offset: if let Some(ShapeEntry::Method { offset }) = e {
                        Some(*offset)
                    } else {
                        None
                    },
                };
                shape.entries.insert(name, entry);
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
