#![allow(dead_code)]

use std::{
    cell::Cell,
    fmt::Display,
    hash::Hash,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::{
    common::{DEBUG_PRINT_GC_STATS, DEBUG_STRESS_GC},
    interned_strings::{InternedString, StringInterner},
};

fn allocate<T: Trace>(value: GcInner<T>) -> NonNull<GcInner<u8>> {
    unsafe {
        let allocated: NonNull<GcInner<T>> =
            NonNull::new(libc::malloc(std::mem::size_of::<GcInner<T>>()).cast())
                .expect("Allocation failed");
        allocated.as_ptr().write(value);
        allocated.cast()
    }
}

fn free(value: NonNull<GcInner<u8>>) {
    unsafe {
        libc::free(value.as_ptr().cast());
    }
}

fn allocation_size(value: NonNull<GcInner<u8>>) -> usize {
    unsafe { libc::malloc_usable_size(value.as_ptr().cast()) }
}

pub struct GcFreeHandle<'a, StringType: InternedString> {
    inner: NonNull<GcInner<u8>>,
    interner: &'a mut StringInterner<StringType>,
}

type FreeGarbage<StringType> = fn(GcFreeHandle<'_, StringType>);

impl<'a, StringType: InternedString> GcFreeHandle<'a, StringType> {
    pub unsafe fn get<T: Trace>(&self) -> &T {
        &self.inner.cast::<GcInner<T>>().as_ref().inner
    }

    pub unsafe fn free<T: Trace>(self) {
        std::ptr::drop_in_place(self.inner.cast::<GcInner<T>>().as_ptr());
    }

    pub unsafe fn free_interned_string(self) {
        self.interner
            .delete(&self.inner.cast::<GcInner<StringType>>().as_ref().inner);
        self.free::<StringType>()
    }
}

pub struct GcRef<T: Trace> {
    value: NonNull<T>,
    borrow: *const Cell<BorrowFlag>,
}

impl<T: Trace> GcRef<T> {
    pub fn map<U: Trace, F>(orig: GcRef<T>, f: F) -> GcRef<U>
    where
        F: FnOnce(&T) -> &U,
    {
        let mapped = GcRef {
            value: f(&*orig).into(),
            borrow: orig.borrow,
        };
        std::mem::forget(orig);
        mapped
    }
}

impl<T: Trace> GcRef<T> {
    pub fn new(value: &GcCell<T>) -> Self {
        let flag = unsafe { &value.inner.as_ref().borrow_flag };
        let mut inner_flag = flag.take();
        inner_flag.borrow();
        flag.set(inner_flag);

        Self {
            borrow: flag,
            value: unsafe { NonNull::from(&value.inner.as_ref().inner).cast() },
        }
    }
}

impl<'a, T: Trace> Deref for GcRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<T: Trace> Drop for GcRef<T> {
    fn drop(&mut self) {
        unsafe { &*self.borrow }.update(|mut flag| {
            flag.release_borrow();
            flag
        });
    }
}

pub struct GcRefMut<T: Trace> {
    value: NonNull<T>,
    borrow: *const Cell<BorrowFlag>,
}

impl<T: Trace> GcRefMut<T> {
    pub fn new(value: &mut GcCell<T>) -> Self {
        let flag = unsafe { &value.inner.as_ref().borrow_flag };
        let mut inner_flag = flag.take();
        inner_flag.borrow_mut();
        flag.set(inner_flag);

        Self {
            borrow: flag,
            value: unsafe { NonNull::from(&value.inner.as_ref().inner).cast() },
        }
    }
}

impl<T: Trace> Deref for GcRefMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<T: Trace> DerefMut for GcRefMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut() }
    }
}

impl<T: Trace> Drop for GcRefMut<T> {
    fn drop(&mut self) {
        unsafe { &*self.borrow }.update(|mut flag| {
            flag.release_mut_borrow();
            flag
        });
    }
}

pub trait Trace {
    fn trace(&self);
}

impl<T: Trace> Trace for GcCell<T> {
    fn trace(&self) {
        if !unsafe { self.inner.as_ref() }.is_marked.get() {
            unsafe { self.inner.as_ref() }.trace();
            // FIXME: Will not work with root being managed by the GC (root is likely mutably borrowed)
            // self.borrow().deref().trace();
            unsafe {
                self.inner.cast::<GcInner<T>>().as_ref().inner.trace();
            }
        }
    }
}

impl<T: Trace, const SIZE: usize> Trace for [T; SIZE] {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}

impl<T: Trace> Trace for Box<[T]> {
    fn trace(&self) {
        for item in self.iter() {
            item.trace();
        }
    }
}

impl<T: Trace> Trace for [T] {
    fn trace(&self) {
        for item in self.iter() {
            item.trace();
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn trace(&self) {
        if let Some(traceable) = self {
            traceable.trace();
        }
    }
}

impl<T: NoTrace> Trace for T {
    fn trace(&self) {
        // Do nothing.
    }
}

pub trait NoTrace {}

impl NoTrace for () {}
impl NoTrace for u8 {}
impl NoTrace for i32 {}
impl NoTrace for String {}

#[derive(Default, Clone, Copy)]
struct BorrowFlag {
    flag: usize,
}

const BORROWED_MUT: usize = usize::MAX;
const UNUSED: usize = 0;

impl BorrowFlag {
    fn can_borrow(&self) -> bool {
        self.flag < BORROWED_MUT - 1
    }

    fn can_borrow_mut(&self) -> bool {
        self.flag == UNUSED
    }

    fn borrow(&mut self) {
        if self.can_borrow() {
            self.flag += 1;
        } else {
            panic!("Cannot borrow GcCell that is already mutably borrowed.");
        }
    }

    fn borrow_mut(&mut self) {
        if self.can_borrow_mut() {
            self.flag = BORROWED_MUT;
        } else {
            panic!("Cannot mutably borrow GcCell that is already borrowed.");
        }
    }

    fn release_mut_borrow(&mut self) {
        assert_eq!(self.flag, BORROWED_MUT);
        self.flag = UNUSED;
    }

    fn release_borrow(&mut self) {
        assert_ne!(self.flag, UNUSED);
        assert_ne!(self.flag, BORROWED_MUT);
        self.flag -= 1;
    }

    fn is_unused(&self) -> bool {
        self.flag == UNUSED
    }
}

// This is actually dynamically sized. By putting inner last we can access other fields just fine.
#[repr(C)]
struct GcInner<T: Trace> {
    next: Option<NonNull<GcInner<u8>>>,
    is_marked: Cell<bool>,
    borrow_flag: Cell<BorrowFlag>,
    inner: T,
}

impl<T: Trace> GcInner<T> {
    fn is_in_use(&self) -> bool {
        self.is_marked.get() || !self.borrow_flag.get().is_unused()
    }
}

impl<T: Trace> Trace for GcInner<T> {
    fn trace(&self) {
        self.is_marked.set(true);
    }
}

#[derive(Debug)]
pub struct GcCell<T>
where
    T: Trace,
{
    inner: NonNull<GcInner<u8>>,
    _phantom_data: PhantomData<T>,
}

impl<T: Trace> GcCell<T> {
    pub fn new<R: Trace>(value: T, gc: &mut GarbageCollector<R, impl InternedString>) -> Self {
        let inner = gc.register(value);
        Self {
            inner,
            _phantom_data: PhantomData,
        }
    }

    pub unsafe fn cast<N: Trace>(self) -> GcCell<N> {
        GcCell {
            inner: self.inner.cast(),
            _phantom_data: PhantomData,
        }
    }

    pub fn borrow<'a>(&'a self) -> GcRef<T> {
        GcRef::new(self)
    }

    pub fn borrow_mut<'a>(&'a mut self) -> GcRefMut<T> {
        GcRefMut::new(self)
    }

    pub fn to_bits(&self) -> usize {
        self.inner.as_ptr() as _
    }

    pub unsafe fn from_bits(value: usize) -> Self {
        Self {
            inner: NonNull::new_unchecked(value as _),
            _phantom_data: PhantomData,
        }
    }
}

impl<T: Hash + Trace> Hash for GcCell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.borrow().hash(state);
    }
}

impl<T: PartialEq + Trace> PartialEq for GcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.borrow().eq(&other.borrow())
    }
}

impl<T: Eq + Trace> Eq for GcCell<T> {}

impl<T: Trace> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _phantom_data: self._phantom_data.clone(),
        }
    }
}

impl<T: Trace> Copy for GcCell<T> {}

impl<T: Trace + Display> Display for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow().fmt(f)
    }
}

// TODO: Only allow one type of object in the GC to simplify the implementation.
pub struct GarbageCollector<Root: Trace, StringType: InternedString> {
    allocated: usize,
    collection_threshold: usize,
    pub root: GcCell<Root>,
    free_contents: FreeGarbage<StringType>,
    pub enabled: bool,
    interned_strings: StringInterner<StringType>,
}

impl<Root: Trace, StringType: InternedString> GarbageCollector<Root, StringType> {
    pub fn new(root: Root, free_contents: FreeGarbage<StringType>) -> Self {
        Self {
            allocated: 0,
            collection_threshold: 1024 * 1024,
            root: GcCell {
                inner: allocate(GcInner {
                    inner: root,
                    next: None,
                    is_marked: Cell::new(false),
                    borrow_flag: Default::default(),
                }),
                _phantom_data: PhantomData,
            },
            free_contents,
            enabled: true,
            interned_strings: StringInterner::new(),
        }
    }

    pub fn intern_string(&mut self, string: StringType) -> GcCell<StringType> {
        if let Some(entry) = self.interned_strings.get(&string) {
            entry
        } else {
            let value = GcCell::new(string, self);
            self.interned_strings.insert(value);
            value
        }
    }

    fn register<T: Trace>(&mut self, value: T) -> NonNull<GcInner<u8>> {
        if self.allocated > self.collection_threshold || DEBUG_STRESS_GC {
            self.collect();
            self.collection_threshold = (self.allocated * 2).max(1024 * 1024);
        }

        let mut boxed = allocate(GcInner {
            inner: value,
            next: None,
            is_marked: Cell::new(false),
            borrow_flag: Default::default(),
        });

        let items = unsafe { &mut self.root.inner.as_mut().next };

        unsafe {
            boxed.as_mut().next = *items;
        }
        *items = Some(boxed);

        self.allocated += allocation_size(boxed);

        *items.as_mut().unwrap()
    }

    fn trace(&self) {
        self.root.trace();
    }

    fn sweep(&mut self) {
        let previous_allocated_size = self.allocated;
        let mut freed_objects = 0;

        let mut obj = self.root.inner;
        while let Some(next) = unsafe { obj.as_mut().next } {
            if unsafe { next.as_ref() }.is_in_use() {
                unsafe { next.as_ref() }.is_marked.set(false);

                obj = next;
            } else {
                (self.free_contents)(GcFreeHandle {
                    inner: next,
                    interner: &mut self.interned_strings,
                });

                self.allocated -= allocation_size(next);
                freed_objects += 1;
                let next = next;
                unsafe { obj.as_mut().next = next.as_ref().next };

                free(next);
            }
        }
        unsafe { self.root.inner.as_ref() }.is_marked.set(false);

        if DEBUG_PRINT_GC_STATS {
            println!(
                "Performed gc: freed {freed_objects} objects ({} of {} bytes)",
                previous_allocated_size - self.allocated,
                previous_allocated_size
            );
        }
    }

    fn collect(&mut self) {
        if !self.enabled {
            return;
        }
        // 1. Trace
        self.trace();
        // 2. Sweep
        self.sweep()
    }

    /*fn intern_string(&mut self, string: StringType) -> GcCell<StringType> {
        if let Some(value) = self.interned_strings.get(&string) {
            value.clone()
        } else {
            let value: GcCell<StringType> = GcCell {
                inner: allocate(GcInner {
                    next: None,
                    is_marked: false,
                    inner: string,
                }),
                _phantom_data: PhantomData,
            };
            self.interned_strings.insert(value.clone());
            value
        }
    }*/
}

#[cfg(test)]
mod test {
    use crate::object::ObjString;

    use super::*;

    #[test]
    fn basic() {
        // Register the unit type as a root. A collection should free all registered values.
        let mut gc: GarbageCollector<[i32; 1024], ObjString> =
            GarbageCollector::new([0; 1024], |free_handle| {
                assert_eq!(unsafe { free_handle.get::<[i32; 25]>() }, &[42; 25])
            });

        let value = GcCell::new([42; 25], &mut gc);
        // Before triggering a gc, accessing the value should be safe.
        assert_eq!(*value.borrow(), [42; 25]);
        // The GC keeps track of the amount of allocated values.
        assert_eq!(gc.allocated, allocation_size(value.inner));
        // Trigger a collection.
        gc.collect();
        // Our allocation has been collected.
        assert_eq!(gc.allocated, 0);
    }

    #[test]
    fn automatic_collection() {
        // Register the unit type as a root. A collection should free all registered values.
        let mut gc: GarbageCollector<(), ObjString> =
            GarbageCollector::new((), |free_handle| unsafe {
                free_handle.free::<[i32; 1024]>()
            });
        for _ in 0..1024 * 1024 {
            let value = GcCell::new([0u8; 1024], &mut gc);
            assert_eq!(*value.borrow(), [0; 1024]);
        }

        assert!(gc.allocated < 1024 * 1024 * 1024);
    }

    #[test]
    fn roots() {
        struct Root {
            roots: Vec<GcCell<u8>>,
        }

        impl Trace for Root {
            fn trace(&self) {
                for root in &self.roots {
                    root.trace();
                }
            }
        }

        let mut gc: GarbageCollector<Root, ObjString> =
            GarbageCollector::new(Root { roots: vec![] }, |_| {});

        let value = GcCell::new(42, &mut gc);
        // Before triggering a gc, accessing the value should be safe.
        assert_eq!(*value.borrow(), 42);
        // The GC keeps track of the amount of allocated values.
        assert_eq!(gc.allocated, allocation_size(value.inner));
        // The value is now a root.
        gc.root.borrow_mut().roots.push(value);
        // Trigger a collection.
        gc.collect();
        // The value is still alive because it is a root.
        assert_eq!(gc.allocated, allocation_size(value.inner));
        // The value is now no longer a root.
        gc.root.borrow_mut().roots.clear();
        // Trigger a collection.
        gc.collect();
        // Our allocation has been collected.
        assert_eq!(gc.allocated, 0);
    }
}
