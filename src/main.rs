#![feature(ptr_sub_ptr)]
#![feature(let_else)]
#![feature(hash_set_entry)]
#![feature(result_flattening)]
#![feature(try_blocks)]
#![feature(cell_update)]

#[macro_use]
extern crate lazy_static;

use std::{ffi::OsString, io::Write, time::Instant};

use vm::Vm;

mod chunk;
mod common;
mod compiler;
mod interned_strings;
mod object;
mod scanner;
mod value;
mod vm;
mod errors;
mod gc;

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
    let mut vm = Vm::new();
    print!("> ");
    std::io::stdout().flush().unwrap();
    for line in std::io::stdin().lines().flatten() {
        vm.borrow_mut().interpret(&line);
        print!("> ");
        std::io::stdout().flush().unwrap();
    }
}

fn runfile(path: OsString) {
    let mut vm = Vm::new();
    let Ok(source) = std::fs::read_to_string(&path) else {
        eprintln!("Could not read file {}", path.to_string_lossy());
        std::process::exit(74);
    };
    let result = vm.borrow_mut().interpret(&source);
    std::io::stdout().flush().unwrap();
    match result {
        vm::InterpretResult::CompileError => std::process::exit(65),
        vm::InterpretResult::RuntimeError => std::process::exit(70),
        vm::InterpretResult::Ok => {}
    }
}
