#![feature(ptr_sub_ptr)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]
#![feature(vec_into_raw_parts)]
#![feature(link_llvm_intrinsics)]
#![allow(clippy::fn_to_numeric_cast)]

use std::sync::Mutex;
use std::{
    collections::HashMap,
    ffi::OsString,
    io::{BufReader, Stdin, Write},
    time::Instant,
};

use emitter::Emitter;
use once_cell::sync::Lazy;

use crate::vm::interpret;

mod common;
mod compiler;
mod emitter;
mod gc;
mod object;
mod properties;
mod scanner;
mod source_mapping;
mod value;
mod vm;

pub static START: Lazy<Instant> = Lazy::new(Instant::now);

// Used when *not* running in REPL mode.
pub static INPUT_STREAM: Lazy<Mutex<BufReader<Stdin>>> =
    Lazy::new(|| Mutex::new(BufReader::new(std::io::stdin())));

fn main() {
    // initialize START
    _ = *START;
    let args = std::env::args_os();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        runfile(args.last().unwrap());
    } else {
        eprintln!("Usage: loxjit [path]");
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
        // This is due to dynasm not getting to see some labels
        print!("> ");
    }
}

fn runfile(path: OsString) {
    let Ok(source) = std::fs::read_to_string(&path) else {
        eprintln!("Could not read file {}", path.to_string_lossy());
        std::process::exit(74);
    };
    let result = interpret(&source, &mut Emitter::new(), &mut Default::default());
    match result {
        vm::InterpretResult::CompileError => std::process::exit(65),
        vm::InterpretResult::RuntimeError => std::process::exit(70),
        vm::InterpretResult::Ok => {}
    }
}
