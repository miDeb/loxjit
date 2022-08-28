#![feature(ptr_sub_ptr)]
#![feature(let_else)]
#![feature(hash_set_entry)]

use std::{ffi::OsString, io::Write};

use vm::Vm;

mod chunk;
mod common;
mod compiler;
mod scanner;
mod value;
mod vm;
mod object;
mod interned_strings;

fn main() {
    let args = std::env::args_os();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        runfile(args.last().unwrap());
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }

    let mut vm = Vm::new();
    vm.run();
}

fn repl() {
    let mut vm = Vm::new();
    print!("> ");
    std::io::stdout().flush().unwrap();
    for line in std::io::stdin().lines().flatten() {
        vm.interpret(&line);
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
    let result = vm.interpret(&source);
    match result {
        vm::InterpretResult::CompileError => std::process::exit(65),
        vm::InterpretResult::RuntimeError => std::process::exit(70),
        vm::InterpretResult::Ok => {}
    }
}
