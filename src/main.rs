#![feature(ptr_sub_ptr)]
#![feature(let_chains)]
#![feature(slice_from_ptr_range)]
#![feature(vec_into_raw_parts)]
#![feature(link_llvm_intrinsics)]
#![allow(clippy::fn_to_numeric_cast)]

use std::io::BufRead;
use std::sync::atomic::{AtomicBool, Ordering};
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
mod assembler;
mod builtins;

pub static START: Lazy<Instant> = Lazy::new(Instant::now);

pub static INPUT_STREAM: Lazy<Mutex<BufReader<Stdin>>> =
    Lazy::new(|| Mutex::new(BufReader::new(std::io::stdin())));
pub static REPL_IGNORE_NEXT_NEWLINE: AtomicBool = AtomicBool::new(false);

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
    let mut buffer = String::new();
    while INPUT_STREAM.lock().unwrap().read_line(&mut buffer).is_ok() {
        if REPL_IGNORE_NEXT_NEWLINE.load(Ordering::Relaxed) {
            REPL_IGNORE_NEXT_NEWLINE.store(false, Ordering::Relaxed);
            buffer.clear();
            continue;
        }
        interpret(&buffer, &mut emitter, &mut globals);
        print!("> ");
        std::io::stdout().flush().unwrap();
        buffer.clear();
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
