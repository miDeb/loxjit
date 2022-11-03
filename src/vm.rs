use std::collections::HashMap;

use crate::{
    compiler::Parser,
    emitter::{Emitter, GlobalVarIndex},
    gc::GcCell,
    object::ObjString,
};

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub fn interpret(
    source: &str,
    emitter: &mut Emitter,
    globals: &mut HashMap<GcCell<ObjString>, GlobalVarIndex>,
) -> InterpretResult {
    let parser = Parser::new(source, emitter, globals);
    match parser.compile() {
        Err(_) => InterpretResult::CompileError,
        Ok(_) => {
            if emitter.run() {
                InterpretResult::Ok
            } else {
                InterpretResult::RuntimeError
            }
        }
    }
}
