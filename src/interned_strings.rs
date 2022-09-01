use rustc_hash::FxHashSet;

use crate::object::Obj;

pub struct StringInterner {
    strings: FxHashSet<Box<Obj>>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: FxHashSet::default(),
        }
    }
    pub fn put(&mut self, string: String) -> *const Obj {
        Box::as_ref(self.strings.get_or_insert(Box::new(string.into())))
    }
}
