use std::borrow::{Borrow, BorrowMut};
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Range;
use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use once_cell::sync::Lazy;
use rustc_hash::FxHashSet;

use crate::common::{DEBUG_PRINT_GC_STATS, DEBUG_STRESS_GC};
use crate::properties::ShapeEntry;
use crate::{
    object::{
        ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjHeader, ObjInstance, ObjString,
        ObjType, ObjUpvalue,
    },
    value::Value,
};

static mut GLOBAL_GC: Lazy<GC> = Lazy::new(GC::new);

pub fn intern_const_string(string: String) -> GcCell<ObjString> {
    unsafe { GLOBAL_GC.intern_string(string, None, true) }
}
pub fn intern_string(string: String, stack: Range<*const Value>) -> GcCell<ObjString> {
    unsafe { GLOBAL_GC.intern_string(string, Some(stack), false) }
}
pub fn register_object<T>(val: T, stack: Range<*const Value>) -> GcCell<T> {
    unsafe { GLOBAL_GC.register_object(val, Some(stack), false) }
}
pub fn register_const_object<T>(val: T) -> GcCell<T> {
    unsafe { GLOBAL_GC.register_object(val, None, true) }
}
#[allow(dead_code)]
pub fn register_object_no_gc<T>(val: T) -> GcCell<T> {
    unsafe { GLOBAL_GC.register_object(val, None, false) }
}

#[repr(u8)]
enum GcFlags {
    Unmarked,
    Marked,
    Constant,
}

pub struct GC {
    first: Option<NonNull<GcInner<ObjHeader>>>,
    occupied: usize,
    next_gc: usize,
    gray_objects: Vec<Value>,
    interned_strings: FxHashSet<GcCell<ObjString>>,
}

impl GC {
    fn new() -> Self {
        Self {
            first: None,
            occupied: 0,
            next_gc: 1028 * 1028,
            gray_objects: Vec::new(),
            interned_strings: FxHashSet::default(),
        }
    }

    fn intern_string(
        &mut self,
        string: String,
        stack: Option<Range<*const Value>>,
        is_const: bool,
    ) -> GcCell<ObjString> {
        self.occupied += std::mem::size_of::<GcInner<ObjString>>();

        if let Some(stack) = stack && (self.occupied > self.next_gc || DEBUG_STRESS_GC) {
            self.garbage_collect(stack);
        }

        let obj = ObjString::new(string);
        if let Some(obj) = self.interned_strings.get(&obj) {
            if is_const {
                obj.to_inner().flag = GcFlags::Constant;
            }

            *obj
        } else {
            let mut inner = Box::new(GcInner {
                flag: if is_const {
                    GcFlags::Constant
                } else {
                    GcFlags::Unmarked
                },
                next: None,
                value: obj,
            });
            let value = GcCell(NonNull::from(&mut inner.value));
            Box::leak(inner);
            self.interned_strings.insert(value);
            value
        }
    }

    fn register_object<T>(
        &mut self,
        value: T,
        stack: Option<Range<*const Value>>,
        is_const: bool,
    ) -> GcCell<T> {
        self.occupied += std::mem::size_of::<GcInner<T>>();

        if let Some(stack) = stack && (self.occupied > self.next_gc || DEBUG_STRESS_GC) {
            self.garbage_collect(stack);
        }

        let mut next = Box::new(GcInner {
            flag: if is_const {
                GcFlags::Constant
            } else {
                GcFlags::Unmarked
            },
            next: self.first,
            value,
        });
        let ptr = NonNull::from(&mut next.value);

        self.first = Some(NonNull::from(Box::leak(next)).cast());

        GcCell(ptr)
    }

    fn sweep(&mut self) {
        let initially_occupied = self.occupied;
        let mut next = &mut self.first;
        while let Some(mut obj) = *next {
            let obj = unsafe { obj.as_mut() };
            match obj.flag {
                GcFlags::Unmarked => {
                    *next = obj.next;
                    Self::free_object_contents(&mut self.occupied, obj);
                }
                GcFlags::Marked => {
                    next = &mut obj.next;
                    obj.flag = GcFlags::Unmarked;
                }
                GcFlags::Constant => {
                    next = &mut obj.next;
                }
            }
        }
        self.interned_strings.retain(|element| {
            let inner = element.to_inner();
            match inner.flag {
                GcFlags::Unmarked => {
                    Self::free_object_contents(&mut self.occupied, inner);
                    false
                }
                GcFlags::Marked => {
                    inner.flag = GcFlags::Unmarked;
                    true
                }
                GcFlags::Constant => true,
            }
        });
        if DEBUG_PRINT_GC_STATS {
            let freed = initially_occupied - self.occupied;
            eprintln!("freed {freed} bytes, {} remaining", self.occupied);
        }
    }

    fn free_object_contents(occupied: &mut usize, obj: &mut GcInner<ObjHeader>) {
        let ptr = obj as *mut _;
        unsafe {
            match obj.value.obj_type {
                ObjType::Function => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjFunction>));
                    *occupied -= std::mem::size_of::<GcInner<ObjFunction>>();
                }

                ObjType::String => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjString>));
                    *occupied -= std::mem::size_of::<GcInner<ObjString>>();
                }
                ObjType::Closure => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjClosure>));
                    *occupied -= std::mem::size_of::<GcInner<ObjClosure>>();
                }
                ObjType::Upvalue => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjUpvalue>));
                    *occupied -= std::mem::size_of::<GcInner<ObjUpvalue>>();
                }
                ObjType::Class => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjClass>));
                    *occupied -= std::mem::size_of::<GcInner<ObjClass>>();
                }
                ObjType::Instance => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjInstance>));
                    *occupied -= std::mem::size_of::<GcInner<ObjInstance>>();
                }
                ObjType::BoundMethod => {
                    drop(Box::from_raw(ptr as *mut GcInner<ObjBoundMethod>));
                    *occupied -= std::mem::size_of::<GcInner<ObjBoundMethod>>();
                }
                ObjType::Shape => {
                    unreachable!()
                }
            }
        }
    }

    fn garbage_collect(&mut self, stack: Range<*const Value>) {
        self.mark_roots(stack);
        while let Some(val) = self.gray_objects.pop() {
            self.trace_value(val);
        }
        self.sweep();
        self.next_gc = (self.occupied * 2).max(1028 * 1028);
    }

    fn mark_roots(&mut self, stack: Range<*const Value>) {
        self.gray_objects.clear();
        // TODO: global variables
        //self.gray_objects.extend_from_slice(global_vars());
        let stack = unsafe { std::slice::from_ptr_range(stack) };
        self.gray_objects.extend_from_slice(stack);
    }

    fn trace_value(&mut self, val: Value) {
        if val.is_obj() {
            let obj = val.as_obj();
            let inner = obj.to_inner();
            match inner.flag {
                GcFlags::Unmarked => inner.flag = GcFlags::Marked,
                GcFlags::Marked | GcFlags::Constant => return,
            }
            match obj.obj_type {
                ObjType::Function | ObjType::String => {}
                ObjType::Closure => {
                    let closure = obj.as_obj_closure();
                    let upvalues = unsafe {
                        Vec::from_raw_parts(
                            closure.upvalues.0,
                            closure.upvalues.1,
                            closure.upvalues.2,
                        )
                    };
                    self.gray_objects
                        .extend(upvalues.iter().map(|v| Value::from(*v)));
                    std::mem::forget(upvalues);
                }
                ObjType::Upvalue => {
                    let upvalue = obj.as_obj_upvalue();
                    if upvalue.location as *const _ == upvalue.closed.as_ptr() {
                        self.gray_objects
                            .push(unsafe { upvalue.closed.assume_init() })
                    }
                }
                ObjType::Class => {
                    let class = obj.as_obj_class();
                    class.name.to_inner().mark();
                    self.gray_objects.push(class.shape.into());
                    self.gray_objects
                        .extend(class.shape.entries.values().filter_map(|e| match e {
                            ShapeEntry::Method { closure }
                            | ShapeEntry::MissingWithKnownShape {
                                method_closure: Some(closure),
                                ..
                            } => Some(Value::from(closure)),
                            _ => None,
                        }));
                }
                ObjType::Instance => {
                    let instance = obj.as_obj_instance();
                    self.gray_objects.push(instance.class.into());
                    self.gray_objects
                        .extend_from_slice(instance.fields.as_slice());
                }
                ObjType::BoundMethod => {
                    let obj_bound_method = obj.as_obj_bound_method();
                    self.gray_objects.push(obj_bound_method.receiver);
                }
                ObjType::Shape => {
                    unreachable!()
                }
            }
        }
    }
}

#[repr(C)]
struct GcInner<T> {
    flag: GcFlags,
    next: Option<NonNull<GcInner<ObjHeader>>>,
    value: T,
}

impl<T> GcInner<T> {
    fn mark(&mut self) {
        match self.flag {
            GcFlags::Unmarked => self.flag = GcFlags::Marked,
            GcFlags::Marked | GcFlags::Constant => {}
        }
    }
}

#[repr(transparent)]
pub struct GcCell<T>(NonNull<T>);

impl<T> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for GcCell<T> {}

impl<T> GcCell<T> {
    fn to_inner(self) -> &'static mut GcInner<ObjHeader> {
        unsafe { &mut *self.0.as_ptr().cast::<u8>().sub(16).cast() }
    }

    pub unsafe fn cast<U>(&self) -> GcCell<U> {
        GcCell(self.0.cast())
    }

    pub unsafe fn from_bits(bits: u64) -> Self {
        Self(NonNull::new_unchecked(bits as *mut T))
    }

    pub fn to_bits(self) -> u64 {
        self.0.as_ptr() as u64
    }

    pub fn as_ptr(&self) -> *const T {
        self.0.as_ptr()
    }

    pub fn as_non_null(&self) -> NonNull<T> {
        self.0
    }
}

impl<T> Borrow<T> for GcCell<T> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<T> BorrowMut<T> for GcCell<T> {
    fn borrow_mut(&mut self) -> &mut T {
        self
    }
}

impl<T> Deref for GcCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T> DerefMut for GcCell<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T: Hash> Hash for GcCell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T: PartialEq> PartialEq for GcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.deref().eq(other)
    }
}

impl<T: Eq> Eq for GcCell<T> {}

impl<T: Display> Display for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}
