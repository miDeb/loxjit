#![feature(ptr_sub_ptr)]
#![feature(hash_set_entry)]
#![feature(result_flattening)]
#![feature(try_blocks)]
#![feature(cell_update)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]
#![feature(vec_into_raw_parts)]

#[macro_use]
extern crate lazy_static;

use std::{collections::HashMap, ffi::OsString, io::Write, time::Instant};

use emitter::Emitter;

use crate::vm::interpret;

mod chunk;
mod common;
mod compiler;
mod emitter;
mod errors;
mod gc;
mod object;
mod scanner;
mod value;
mod vm;

lazy_static! {
    pub static ref START: Instant = Instant::now();
}

fn main() {
    // initialize START
    _ = *START;
    let args = std::env::args_os();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        runfile(args.last().unwrap());
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }
}

fn repl() {
    print!("> ");
    std::io::stdout().flush().unwrap();
    let mut emitter = Emitter::new();
    let mut globals = HashMap::new();
    for line in std::io::stdin().lines().flatten() {
        interpret(&line, &mut emitter, &mut globals);
        // FIXME: In case of an error the compiler/emitter are left in a bad state
        // and produce wrong results in subsequent invocations.
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}

fn runfile(path: OsString) {
    let Ok(source) = std::fs::read_to_string(&path) else {
        eprintln!("Could not read file {}", path.to_string_lossy());
        std::process::exit(74);
    };
    let result = interpret(&source, &mut Emitter::new(), &mut Default::default());
    std::io::stdout().flush().unwrap();
    match result {
        vm::InterpretResult::CompileError => std::process::exit(65),
        vm::InterpretResult::RuntimeError => std::process::exit(70),
        vm::InterpretResult::Ok => {}
    }
}
