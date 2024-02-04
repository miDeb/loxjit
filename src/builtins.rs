use crate::{
    emitter::{Emitter, GlobalVarIndex},
    gc::intern_string,
    object::ObjString,
    value::Value,
};

pub extern "win64" fn print(value: Value) {
    print!("{}", value)
}

pub extern "win64" fn handle_global_uninit(
    rip: *const u8,
    rbp: *const u8,
    emitter: *const Emitter,
    index: GlobalVarIndex,
) {
    let emitter = unsafe { &*emitter };
    let name = emitter.get_global_name(index);
    eprintln!("Undefined variable '{}'.", name);
    emitter.print_stacktrace(rip, rbp);
}
pub extern "win64" fn handle_unexpected_add_operands(
    rip: *const u8,
    rbp: *const u8,
    emitter: *const Emitter,
) {
    eprintln!("Operands must be two numbers or two strings.");
    unsafe { &*emitter }.print_stacktrace(rip, rbp);
}

pub extern "win64" fn concat_strings(
    stack_start: *const Value,
    stack_end: *const Value,
    left: Value,
    right: Value,
) -> Value {
    if !left.is_obj_string() || !right.is_obj_string() {
        // return 0 to signal failure.
        0f64.into()
    } else {
        let left_str = &left.as_obj_string().string;
        let right_str = &right.as_obj_string().string;
        let mut concatenated = String::with_capacity(left_str.len() + right_str.len());
        concatenated.push_str(left_str);
        concatenated.push_str(right_str);
        intern_string(concatenated, stack_start..stack_end).into()
    }
}
