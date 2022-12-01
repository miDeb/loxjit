use std::{ops::Range, ptr::NonNull, sync::atomic::AtomicUsize};

use rustc_hash::FxHashMap;

use crate::{
    gc::{register_object, GcCell},
    object::{ObjHeader, ObjString},
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

// 0 is a reserved shape id
static SHAPE_ID: AtomicUsize = AtomicUsize::new(1);

#[repr(C)]
#[derive(Clone)]
pub struct ObjShape {
    header: ObjHeader,
    pub id: usize,
    pub entries: FxHashMap<NonNull<ObjString>, ShapeEntry>,
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
            parent_shape.entries.remove(&parent_key.as_non_null());
        }
    }

    fn new(parent: Option<(GcCell<ObjShape>, GcCell<ObjString>)>) -> Self {
        Self {
            header: Self::header(),
            id: SHAPE_ID.fetch_add(1, std::sync::atomic::Ordering::AcqRel),
            parent,
            entries: if let Some(parent) = parent {
                parent
                    .0
                    .entries
                    .iter()
                    .filter_map(|(&name, &entry)| match entry {
                        ShapeEntry::MissingWithKnownShape {
                            method_offset: Some(method_offset),
                            ..
                        } => Some((
                            name,
                            ShapeEntry::Method {
                                offset: method_offset,
                            },
                        )),
                        ShapeEntry::MissingWithKnownShape {
                            method_offset: None,
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
                method_offset: Some(offset),
                ..
            }) => Some(ShapeEntry::Method { offset: *offset }),
            Some(e @ ShapeEntry::Method { .. }) | Some(e @ ShapeEntry::Present { .. }) => Some(*e),
            _ => None,
        }
    }

    pub fn add_method(mut shape: GcCell<ObjShape>, name: GcCell<ObjString>, method_len: usize) {
        shape.entries.insert(
            name.as_non_null(),
            ShapeEntry::Method { offset: method_len },
        );
    }

    pub fn resolve_set_property(
        stack: Range<*const Value>,
        mut shape: GcCell<ObjShape>,
        name: GcCell<ObjString>,
        property_len: usize,
    ) -> ShapeEntry {
        match shape.entries.get(&name.as_non_null()) {
            e @ None | e @ Some(ShapeEntry::Method { .. }) => {
                let mut new_shape = register_object(ObjShape::new(Some((shape, name))), stack);
                new_shape.entries.insert(
                    name.as_non_null(),
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
