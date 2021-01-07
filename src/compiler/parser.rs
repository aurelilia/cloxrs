use crate::compiler::scanner::Scanner;
use crate::compiler::token::{Token, Type};
use std::rc::Rc;
use std::mem;
use std::cell::RefCell;
use crate::MutRc;

pub struct Parser {
    pub scanner: Scanner,
    pub previous: Token,
    pub current: Token,

    pub had_error: bool,
    panic_mode: bool,
}

impl Parser {
    pub fn advance(&mut self) {
        loop {
            let tok = self.scanner.scan_token().unwrap_or_else(|| Token::generic_token(Type::EOF));
            self.previous = mem::replace(&mut self.current, tok);

            if let Type::Error = self.current.t_type {
                self.error(&self.current.lexeme.clone());
            } else {
                break;
            }
        }
    }

    pub fn match_next(&mut self, t_type: Type) -> bool {
        if !self.check(t_type) {
            return false;
        }
        self.advance();
        true
    }

    pub fn consume(&mut self, t_type: Type, message: &str) {
        if t_type == self.current.t_type {
            self.advance();
        } else {
            self.error(message);
        }
    }

    pub fn check(&self, t_type: Type) -> bool {
        t_type == self.current.t_type
    }

    pub fn error(&mut self, message: &str) {
        if self.panic_mode {
            return;
        }

        eprint!("[Line {}] Error", self.current.line);
        match self.current.t_type {
            Type::EOF => eprint!(" at end"),
            Type::Error => (),
            _ => eprint!(" at line {}", self.current.line),
        }
        eprintln!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    pub fn synchronize(&mut self) {
        if !self.panic_mode { return }
        self.panic_mode = false;

        while self.current.t_type != Type::EOF {
            if self.previous.t_type == Type::Semicolon {
                return;
            }

            match self.current.t_type {
                Type::Class
                | Type::Fun
                | Type::Var
                | Type::For
                | Type::If
                | Type::While
                | Type::Print
                | Type::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    pub fn new(code: &str) -> MutRc<Parser> {
        Rc::new(RefCell::new(Parser {
            scanner: Scanner::new(code),

            previous: Token::generic_token(Type::Error),
            current: Token::generic_token(Type::Error),

            had_error: false,
            panic_mode: false,
        }))
    }
}
