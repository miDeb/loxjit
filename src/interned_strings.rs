use lazy_static::__Deref;
use rustc_hash::FxHashSet;
use std::{borrow::Borrow, fmt::Display, hash::Hash};

use crate::gc::{GcCell, Trace};

pub struct StringInterner<StringType: InternedString> {
    strings: FxHashSet<GcCell<StringType>>,
}

impl<StringType: InternedString> StringInterner<StringType> {
    pub fn new() -> Self {
        Self {
            strings: FxHashSet::default(),
        }
    }

    pub fn get(&mut self, string: &StringType) -> Option<GcCell<StringType>> {
        self.strings.get(string).map(Clone::clone)
    }

    pub fn insert(&mut self, string: GcCell<StringType>) {
        self.strings.insert(string);
    }

    pub fn delete(&mut self, string: &StringType) {
        self.strings.remove(string);
    }
}

pub trait InternedString: Trace + Hash + Eq + Display {}

impl<StringType: InternedString> Borrow<StringType> for GcCell<StringType> {
    fn borrow(&self) -> &StringType {
        // we need this to convince the HashMap to allow us to access it without creating a GcCell.
        unsafe { std::mem::transmute(self.borrow().deref()) }
    }
}
