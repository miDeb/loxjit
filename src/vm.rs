use crate::compiler::Parser;

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub fn interpret(source: &str) -> InterpretResult {
    let parser = Parser::new(source);
    match parser.compile() {
        Err(_) => InterpretResult::CompileError,
        Ok(emitter) => {
            if emitter.run() {
                InterpretResult::Ok
            } else {
                InterpretResult::RuntimeError
            }
        }
    }
}
