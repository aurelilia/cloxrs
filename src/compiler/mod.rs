mod scanner;
mod token;

use plain_enum::TPlainEnum;

use super::chunk::{OpCodeLine, Chunk};
use super::opcode::OpCode;
use super::value::Value;
use token::*;
use scanner::Scanner;

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    compiling_chunk: &'a mut Chunk,

    previous: Token<'a>,
    current: Token<'a>,

    had_error: bool,
    panic_mode: bool
}

impl<'a> Compiler<'a> {
    pub fn compile(&mut self, source: &'a String) -> bool {
        self.scanner = Scanner::new(source);
        self.advance();
        self.expression();
        self.consume(Type::EOF, "Expected end of expression.");
        self.end_compiliation();
        !self.had_error
    }
    
    fn expression(&mut self) { 
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(Type::RightParen, "Expected ')' after expression.");
    }

    fn unary(&mut self) {
        let op_type = self.previous.t_type;
        self.parse_precedence(Precedence::Unary);

        match op_type {
            Type::Minus => self.emit_opcode(OpCode::Negate),
            Type::Bang => self.emit_opcode(OpCode::Not),
            _ => ()
        }
    }

    fn binary(&mut self) {
        let op_type = self.previous.t_type;


        let rule = Compiler::get_rule(op_type);
        self.parse_precedence(Precedence::from_usize(rule.precedence.to_usize() + 1));

        match op_type {
            Type::Plus => self.emit_opcode(OpCode::Add),
            Type::Minus => self.emit_opcode(OpCode::Substract),
            Type::Star => self.emit_opcode(OpCode::Multiply),
            Type::Slash => self.emit_opcode(OpCode::Divide),

            Type::BangEqual => self.emit_opcodes(OpCode::Equal, OpCode::Not),
            Type::EqualEqual => self.emit_opcode(OpCode::Equal),
            Type::Greater => self.emit_opcode(OpCode::Greater),
            Type::GreaterEqual => self.emit_opcodes(OpCode::Less, OpCode::Not),
            Type::Less => self.emit_opcode(OpCode::Less),
            Type::LessEqual => self.emit_opcodes(OpCode::Greater, OpCode::Not),

            _ => ()
        }
    }

    fn literal(&mut self) {
        match self.previous.t_type {
            Type::False => self.emit_opcode(OpCode::Constant(Value::Bool(false))),
            Type::Nil => self.emit_opcode(OpCode::Constant(Value::Nil)),
            Type::True => self.emit_opcode(OpCode::Constant(Value::Bool(true))),
            Type::Number => {
                let value: f64 = self.previous.lexeme.parse().expect("Invalid number?");
                self.emit_opcode(OpCode::Constant(Value::Number(value)));
            }
            _ => ()
        }
    }

    fn string(&mut self) {
        // Trim leading and trailing "
        let mut string = String::from(&self.previous.lexeme[1..]);
        string.pop();
        self.emit_opcode(OpCode::Constant(Value::String(string)))
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = Compiler::get_rule(self.previous.t_type).prefix;

        if let Option::Some(rule) = prefix_rule { 
            rule(self); 
        } else { 
            self.error_at(self.current, "Expected expression."); 
            return;
        }

        while precedence.to_usize() <= Compiler::get_rule(self.current.t_type).precedence.to_usize() {
            self.advance();
            let infix_rule = Compiler::get_rule(self.previous.t_type).infix.expect("Internal error: Unexpected missing infix operation!!");
            infix_rule(self);
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if let Type::Error = self.current.t_type { () } else { break; } // TODO: Inverted if-let???

            self.error_at_current(self.current.lexeme);
        }
    }

    fn consume(&mut self, t_type: Type, message: &str) {
        if t_type == self.current.t_type { self.advance(); }
        else { self.error_at_current(message); }
    }

    fn emit_opcode(&mut self, code: OpCode) {
        self.compiling_chunk.code.push(OpCodeLine { code, line: self.previous.line })
    }

    fn emit_opcodes(&mut self, code1: OpCode, code2: OpCode) {
        self.emit_opcode(code1);
        self.emit_opcode(code2);
    }

    fn end_compiliation(&mut self) {
        self.emit_opcode(OpCode::Return);
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message)
    }

    fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode { return; }

        eprint!("[Line {}] Error", token.line);
        match token.t_type {
            Type::EOF => eprint!(" at end"),
            Type::Error => (),
            _ => eprint!(" at line {}", token.line)
        }
        eprintln!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn get_rule(t_type: Type) -> &'static ParseRule {
        &RULES[t_type.to_usize()]
    }

    pub fn new(chunk: &'a mut Chunk) -> Compiler<'a> {
        // Note: All struct values are initialized to stub values.
        // TODO: Find a way to create a stubbed chunk reference without having to pass one in
        Compiler {
            scanner: Scanner::new(""),
            compiling_chunk: chunk,

            previous: Token { 
                t_type: Type::Error,
                lexeme: "\0",
                line: 0
            },
            current: Token { 
                t_type: Type::Error,
                lexeme: "\0",
                line: 0
            },

            had_error: false,
            panic_mode: false
        }
    }
}

plain_enum_mod!{this, Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}}

struct ParseRule {
    prefix: Option<fn(&mut Compiler)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: Precedence
}

impl ParseRule {
    const fn new(precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix: Option::None,
            infix: Option::None,
            precedence
        }
    }

    const fn new_both(prefix: fn(&mut Compiler), infix: Option<fn(&mut Compiler)>, precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix: Option::Some(prefix),
            infix,
            precedence
        }
    }

    const fn new_infix(infix: fn(&mut Compiler), precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix: Option::None,
            infix: Option::Some(infix),
            precedence
        }
    }
}

static RULES: [ParseRule; 40] = [                                              
    ParseRule::new_both(|compiler| { compiler.grouping() }, Option::None, Precedence::Call),                                    // LEFT_PAREN      
    ParseRule::new(Precedence::None),                                                                                           // RIGHT_PAREN
    ParseRule::new(Precedence::None),                                                                                           // LEFT_BRACE
    ParseRule::new(Precedence::None),                                                                                           // RIGHT_BRACE     
    ParseRule::new(Precedence::None),                                                                                           // COMMA           
    ParseRule::new(Precedence::Call),                                                                                           // DOT             
    ParseRule::new_both(|compiler| { compiler.unary() }, Option::Some(|compiler| { compiler.binary() }), Precedence::Term),     // MINUS           
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Term),                                                   // PLUS            
    ParseRule::new(Precedence::None),                                                                                           // SEMICOLON       
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Factor),                                                 // SLASH           
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Factor),                                                 // STAR            
    ParseRule::new(Precedence::None),                                                                                           // BANG            
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Equality),                                               // BANG_EQUAL            
    ParseRule::new(Precedence::None),                                                                                           // EQUAL           
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Equality),                                               // EQUAL_EQUAL     
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Comparison),                                             // GREATER         
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Comparison),                                             // GREATER_EQUAL   
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Comparison),                                             // LESS            
    ParseRule::new_infix(|compiler| { compiler.binary() }, Precedence::Comparison),                                             // LESS_EQUAL      
    ParseRule::new(Precedence::None),                                                                                           // IDENTIFIER      
    ParseRule::new_both(|compiler| { compiler.string() }, Option::None, Precedence::Term),                                      // STRING           
    ParseRule::new_both(|compiler| { compiler.literal() }, Option::None, Precedence::None ),                                    // NUMBER          
    ParseRule::new(Precedence::And),                                                                                            // AND             
    ParseRule::new(Precedence::None),                                                                                           // CLASS           
    ParseRule::new(Precedence::None),                                                                                           // ELSE            
    ParseRule::new_both(|compiler| { compiler.literal() }, Option::None, Precedence::None),                                     // FALSE           
    ParseRule::new(Precedence::None),                                                                                           // FOR             
    ParseRule::new(Precedence::None),                                                                                           // FUN             
    ParseRule::new(Precedence::None),                                                                                           // IF              
    ParseRule::new_both(|compiler| { compiler.literal() }, Option::None, Precedence::None),                                     // NIL           
    ParseRule::new(Precedence::Or),                                                                                             // OR              
    ParseRule::new(Precedence::None),                                                                                           // PRINT           
    ParseRule::new(Precedence::None),                                                                                           // RETURN          
    ParseRule::new(Precedence::None),                                                                                           // SUPER           
    ParseRule::new(Precedence::None),                                                                                           // THIS            
    ParseRule::new_both(|compiler| { compiler.literal() }, Option::None, Precedence::None),                                     // TRUE           
    ParseRule::new(Precedence::None),                                                                                           // VAR             
    ParseRule::new(Precedence::None),                                                                                           // WHILE           
    ParseRule::new(Precedence::None),                                                                                           // ERROR           
    ParseRule::new(Precedence::None),                                                                                           // EOF             
];    