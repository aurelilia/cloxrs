#![feature(const_fn)]
#![feature(const_mut_refs)]
#![feature(const_fn_fn_ptr_basics)]

#[macro_use]
extern crate plain_enum;
#[macro_use]
extern crate enum_methods;

mod chunk;
mod compiler;
mod disassembler;
mod opcode;
mod value;
mod vm;

use std::{
    cell::RefCell,
    fs,
    io::{self, Write},
    process,
    rc::Rc,
};
use vm::VM;

type MutRc<T> = Rc<RefCell<T>>;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: loxrs [path]");
            process::exit(64);
        }
    }
}

fn repl() {
    let mut vm = VM::new();
    let mut input = String::new();
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout!");

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line!");

        vm.interpret(&input).ok();
    }
}

fn run_file(path: &str) {
    let mut vm = VM::new();
    let file = fs::read_to_string(path);
    match file {
        Ok(input) => vm.interpret(&input).ok(),
        Err(_) => {
            println!("Failed to read file.");
            process::exit(74);
        }
    };
}
