#![feature(const_fn)]
#![feature(bind_by_move_pattern_guards)]

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

use std::io::Write;
use std::{fs, io, process, mem};
use vm::VM;

fn main() {
    let vm = VM::new();

    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => repl(vm),
        2 => run_file(vm, &args[1]),
        _ => {
            println!("Usage: loxrs [path]");
            process::exit(64);
        }
    }
}

fn repl(mut vm: VM) {
    let mut input = String::new();
    loop {
        print!("> ");
        io::stdout().flush().ok().expect("Failed to flush stdout!");

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line!");

        vm.interpret(mem::replace(&mut input, String::new()));
    }
}

fn run_file(mut vm: VM, path: &String) {
    let file = fs::read_to_string(path);
    match file {
        Ok(input) => vm.interpret(input),
        Err(_) => {
            println!("Failed to read file.");
            process::exit(74);
        }
    };
}
