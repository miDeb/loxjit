use std::collections::HashSet;

use crate::object::Obj;

pub struct StringInterner {
    strings: HashSet<Box<Obj>>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: HashSet::new(),
        }
    }
    pub fn put(&mut self, string: String) -> *const Obj {
        Box::as_ref(self.strings.get_or_insert(Box::new(string.into())))
    }
}
