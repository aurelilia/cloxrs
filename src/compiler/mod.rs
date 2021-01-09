mod parser;
mod scanner;
mod token;

use plain_enum::TPlainEnum;
use smallvec::SmallVec;
use smol_str::SmolStr;

use super::chunk::{Chunk, OpCodeLine};
use super::opcode::OpCode;
use crate::MutRc;
use crate::{compiler::parser::Parser, disassembler::disassemble_chunk};
use crate::{
    opcode::Constant,
    value::{Closure, Function, Upvalue},
    UInt,
};
use std::mem;
use std::rc::Rc;
use std::{
    cell::{Ref, RefCell, RefMut},
    mem::MaybeUninit,
};
use token::*;

#[derive(Debug, PartialEq)]
enum FunctionType {
    Function,
    Method,
    Initializer,
    Script,
}

#[derive(Clone)]
struct ClassCompile {
    name: Token,
    subclass: bool,
}

pub struct Compiler {
    parser: MutRc<Parser>,

    function: Function,
    function_type: FunctionType,

    locals: Vec<Local>,
    scope_depth: usize,
    upvalues: SmallVec<[Upvalue; 3]>,

    enclosing: Option<Box<Compiler>>,
    class_stack: MutRc<Vec<ClassCompile>>,
}

impl Compiler {
    pub fn compile(&mut self) -> Option<Rc<Closure>> {
        self.parser_mut().advance();

        while !self.parser_mut().match_next(Type::EOF) {
            self.declaration();
        }

        let function = self.end_compiliation();
        if self.parser_mut().had_error {
            None
        } else {
            Some(Rc::new(Closure {
                function,
                upvalues: SmallVec::new(),
            }))
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        match () {
            _ if self.parser_mut().match_next(Type::Class) => self.class_declaration(),
            _ if self.parser_mut().match_next(Type::Var) => self.var_declaration(),
            _ if self.parser_mut().match_next(Type::Fun) => self.fun_declaration(),
            _ => self.statement(),
        }

        self.parser_mut().synchronize();
    }

    fn class_declaration(&mut self) {
        self.consume(Type::Identifier, "Expected class name.");
        self.declare_variable();
        let name = self.previous();

        self.emit_opcode(OpCode::Class(name.lexeme.clone()));
        self.define_variable(Some(name.lexeme.clone()));

        let inherit = self.parser_mut().match_next(Type::Less);
        self.class_stack.borrow_mut().push(ClassCompile {
            name: name.clone(),
            subclass: inherit,
        });

        if inherit {
            self.consume(Type::Identifier, "Expected superclass name.");
            let superclass = self.previous();

            if superclass.lexeme == name.lexeme {
                self.error("A class cannot inherit from itself.");
            }
            self.variable(false);

            self.begin_scope();
            self.add_local(Token::generic_ident("super"));
            self.mark_initialized();

            self.named_variable(&superclass, false);
        }
        self.named_variable(&name, false);

        self.consume(Type::LeftBrace, "Expected '{' before class body.");
        while !self.parser().check(Type::RightBrace) && !self.parser().check(Type::EOF) {
            self.method();
        }
        self.consume(Type::RightBrace, "Expected '}' after class body.");

        self.emit_opcode(OpCode::EndClass(inherit));

        if inherit {
            self.end_scope();
        }
        self.class_stack.borrow_mut().pop();
    }

    fn method(&mut self) {
        self.consume(Type::Identifier, "Expected method name.");
        let fn_ty = if self.parser().previous.lexeme == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(fn_ty);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name.");

        if self.parser_mut().match_next(Type::Equal) {
            self.expression();
        } else {
            self.emit_opcode(OpCode::Constant(Constant::Nil));
        }

        self.parser_mut()
            .consume(Type::Semicolon, "Expected ';' after variable declaration.");
        self.define_variable(global);
    }

    // This is a very hacky solution...
    // Since a compiler needs access to its enclosing compiler, self
    // is replaced by an uninit (undefined behavior!!) to get ownership.
    #[allow(clippy::clippy::uninit_assumed_init)]
    #[allow(invalid_value)]
    fn function(&mut self, function_type: FunctionType) {
        let tmp: Compiler = mem::replace(self, unsafe { MaybeUninit::uninit().assume_init() });

        let name = tmp.parser_mut().previous.lexeme.clone();

        let local_name = if function_type != FunctionType::Function {
            "this"
        } else {
            ""
        };
        let mut comp = Compiler {
            parser: Rc::clone(&tmp.parser),
            function: Self::new_function(Some(name), 0),
            function_type,
            locals: vec![Local {
                name: Token::generic_ident(local_name),
                depth: 0,
                initialized: true,
                captured: false,
            }],
            scope_depth: 0,
            upvalues: SmallVec::new(),
            class_stack: Rc::clone(&tmp.class_stack),
            enclosing: Some(Box::new(tmp)),
        };
        comp.begin_scope();

        comp.consume(Type::LeftParen, "Expected '(' after function name.");
        if !comp.parser_mut().check(Type::RightParen) {
            loop {
                comp.function.arity += 1;
                let param = comp.parse_variable("Expected parameter name.");
                comp.define_variable(param);

                if !comp.parser_mut().match_next(Type::Comma) {
                    break;
                }
            }
        }
        comp.consume(Type::RightParen, "Expected ')' after parameters.");

        comp.consume(Type::LeftBrace, "Expected '{' before function body.");
        comp.block();

        let fun = comp.end_compiliation();
        disassemble_chunk(&fun.borrow().chunk, &fun.borrow().name);
        let closure = Closure {
            function: fun,
            upvalues: comp.upvalues,
        };

        let uninitialized = mem::replace(self, *comp.enclosing.unwrap());
        mem::forget(uninitialized);

        self.emit_opcode(OpCode::Closure(Rc::new(closure)));
    }

    fn statement(&mut self) {
        match () {
            _ if self.parser_mut().match_next(Type::Print) => self.print_statement(),
            _ if self.parser_mut().match_next(Type::If) => self.if_statement(),
            _ if self.parser_mut().match_next(Type::While) => self.while_statement(),
            _ if self.parser_mut().match_next(Type::For) => self.for_statement(),
            _ if self.parser_mut().match_next(Type::Return) => self.return_statement(),
            _ if self.parser_mut().match_next(Type::LeftBrace) => {
                self.begin_scope();
                self.block();
                self.end_scope();
            }
            _ => self.expression_statement(),
        };
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(Type::Semicolon, "Expected ';' after value.");
        self.emit_opcode(OpCode::Print);
    }

    fn if_statement(&mut self) {
        self.consume(Type::LeftParen, "Expected '(' after 'if'.");
        self.expression();
        self.consume(Type::RightParen, "Expected ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_opcode(OpCode::Pop);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump(0));
        self.patch_jump(then_jump);
        self.emit_opcode(OpCode::Pop);

        if self.parser_mut().match_next(Type::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();

        self.consume(Type::LeftParen, "Expected '(' after 'while'.");
        self.expression();
        self.consume(Type::RightParen, "Expected ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0));

        self.emit_opcode(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(Type::LeftParen, "Expected '(' after 'for'.");

        if self.parser_mut().match_next(Type::Var) {
            self.var_declaration();
        } else if self.parser_mut().match_next(Type::Semicolon) {
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();

        let mut exit_jump: usize = 0;
        if !self.parser_mut().match_next(Type::Semicolon) {
            self.expression();
            self.consume(Type::Semicolon, "Expected ';' after loop condition.");

            exit_jump = self.emit_jump(OpCode::JumpIfFalse(0));
            self.emit_opcode(OpCode::Pop);
        }

        if !self.parser_mut().match_next(Type::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump(0));

            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit_opcode(OpCode::Pop);
            self.consume(Type::RightParen, "Expected ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        if exit_jump != 0 {
            self.patch_jump(exit_jump);
            self.emit_opcode(OpCode::Pop);
        }

        self.end_scope();
    }

    fn return_statement(&mut self) {
        if self.function_type == FunctionType::Script {
            self.error("Cannot return from top-level code.");
        }

        if self.parser_mut().match_next(Type::Semicolon) {
            self.emit_return();
        } else {
            if self.function_type == FunctionType::Initializer {
                self.error("Cannot return a value from an initializer.");
            }

            self.expression();
            self.consume(Type::Semicolon, "Expected ';' after return value.");
            self.emit_opcode(OpCode::Return);
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(Type::Semicolon, "Expected ';' after expression.");
        self.emit_opcode(OpCode::Pop);
    }

    fn block(&mut self) {
        while !self.parser().check(Type::RightBrace) && !self.parser().check(Type::EOF) {
            self.declaration();
        }

        self.consume(Type::RightBrace, "Expected '}' after block.");
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(Type::RightParen, "Expected ')' after expression.");
    }

    fn unary(&mut self) {
        let op_type = self.previous().t_type;
        self.parse_precedence(Precedence::Unary);

        match op_type {
            Type::Minus => self.emit_opcode(OpCode::Negate),
            Type::Bang => self.emit_opcode(OpCode::Not),
            _ => (),
        }
    }

    fn binary(&mut self) {
        let op_type = self.previous().t_type;

        let rule = Compiler::get_rule(op_type);
        self.parse_precedence(Precedence::from_usize(rule.precedence.to_usize() + 1));

        match op_type {
            Type::Plus => self.emit_opcode(OpCode::Add),
            Type::Minus => self.emit_opcode(OpCode::Subtract),
            Type::Star => self.emit_opcode(OpCode::Multiply),
            Type::Slash => self.emit_opcode(OpCode::Divide),

            Type::BangEqual => self.emit_opcodes(OpCode::Equal, OpCode::Not),
            Type::EqualEqual => self.emit_opcode(OpCode::Equal),
            Type::Greater => self.emit_opcode(OpCode::Greater),
            Type::GreaterEqual => self.emit_opcodes(OpCode::Less, OpCode::Not),
            Type::Less => self.emit_opcode(OpCode::Less),
            Type::LessEqual => self.emit_opcodes(OpCode::Greater, OpCode::Not),

            _ => (),
        }
    }

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_opcode(OpCode::Call(arg_count))
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(Type::Identifier, "Expected property name after '.'.");
        let name = self.previous().lexeme;

        if can_assign && self.parser_mut().match_next(Type::Equal) {
            self.expression();
            self.emit_opcode(OpCode::SetProperty(name))
        } else if self.parser_mut().match_next(Type::LeftParen) {
            let args_count = self.argument_list();
            self.emit_opcode(OpCode::Invoke(name, args_count));
        } else {
            self.emit_opcode(OpCode::GetProperty(name))
        }
    }

    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        let end_jump = self.emit_jump(OpCode::Jump(0));

        self.patch_jump(else_jump);
        self.emit_opcode(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser_mut().advance();
        let prefix_rule = Compiler::get_rule(self.previous().t_type).prefix;
        let can_assign = precedence <= Precedence::Assignment;

        if let Some(rule) = prefix_rule {
            rule(self, can_assign);
        } else {
            self.error("Expected expression.");
            return;
        }

        while precedence.to_usize()
            <= Compiler::get_rule(self.current().t_type)
                .precedence
                .to_usize()
        {
            self.parser_mut().advance();
            let infix_rule = Compiler::get_rule(self.previous().t_type).infix;
            match infix_rule {
                Some(rule) => rule(self, can_assign),
                None => self.error("Unexpected infix expression."),
            }
        }

        if can_assign && self.parser_mut().match_next(Type::Equal) {
            self.error("Invalid assignment target.");
            self.expression();
        }
    }

    fn literal(&mut self) {
        match self.previous().t_type {
            Type::False => self.emit_opcode(OpCode::Constant(Constant::Bool(false))),
            Type::Nil => self.emit_opcode(OpCode::Constant(Constant::Nil)),
            Type::True => self.emit_opcode(OpCode::Constant(Constant::Bool(true))),
            Type::Number => {
                let value: f64 = self.previous().lexeme.parse().expect("Invalid number?");
                self.emit_opcode(OpCode::Constant(Constant::Number(value)));
            }
            _ => (),
        }
    }

    fn string(&mut self) {
        self.emit_opcode(OpCode::Constant(Constant::String(self.previous().lexeme)))
    }

    fn this(&mut self) {
        if self.class_stack.borrow().is_empty() {
            self.error("Cannot use 'this' outside of a class.");
            return;
        }
        self.variable(false);
    }

    fn super_(&mut self) {
        if let Some(cls) = self.class_stack.borrow().last().cloned() {
            if !cls.subclass {
                self.error("Cannot use 'super' in a class with no superclass.");
                return;
            }
        } else {
            self.error("Cannot use 'super' outside of class.");
            return;
        }

        self.consume(Type::Dot, "Expected '.' after 'super'.");
        self.consume(Type::Identifier, "Expected superclass method name.");
        let name = self.previous();
        self.named_variable(&Token::generic_ident("this"), false);
        if self.parser_mut().match_next(Type::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(&Token::generic_ident("super"), false);
            self.emit_opcode(OpCode::InvokeSuper(name.lexeme, arg_count));
        } else {
            self.named_variable(&Token::generic_ident("super"), false);
            self.emit_opcode(OpCode::GetSuper(name.lexeme));
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.previous(), can_assign);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let get_op;
        let set_op;
        let arg = self.resolve_local(&name, false);
        if let Some(arg) = arg {
            get_op = OpCode::GetLocal(arg);
            set_op = OpCode::SetLocal(arg);
        } else if let Some(upvalue) = self.resolve_upvalue(name) {
            get_op = OpCode::GetUpvalue(upvalue);
            set_op = OpCode::SetUpvalue(upvalue);
        } else {
            get_op = OpCode::GetGlobal(name.lexeme.clone());
            set_op = OpCode::SetGlobal(name.lexeme.clone());
        }

        if can_assign && self.parser_mut().match_next(Type::Equal) {
            self.expression();
            self.emit_opcode(set_op);
        } else {
            self.emit_opcode(get_op);
        }
    }

    fn resolve_local(&mut self, name: &Token, capture: bool) -> Option<UInt> {
        for (index, local) in self.locals.iter_mut().enumerate().rev() {
            if name.lexeme == local.name.lexeme {
                if !local.initialized {
                    self.parser
                        .borrow_mut()
                        .error("Cannot read variable in its own initializer.");
                }
                local.captured = local.captured || capture;
                return Some(index as UInt);
            }
        }
        None
    }

    fn resolve_upvalue(&mut self, name: &Token) -> Option<UInt> {
        if let Some(local) = self
            .enclosing
            .as_mut()
            .map(|e| e.resolve_local(name, true))
            .flatten()
        {
            Some(self.add_upvalue(local, true))
        } else if let Some(local) = self
            .enclosing
            .as_mut()
            .map(|e| e.resolve_upvalue(name))
            .flatten()
        {
            Some(self.add_upvalue(local, false))
        } else {
            None
        }
    }

    fn add_upvalue(&mut self, index: UInt, is_local: bool) -> UInt {
        for (i, value) in self.upvalues.iter().enumerate() {
            if value.index == index && value.is_local == is_local {
                return i as UInt;
            }
        }

        self.upvalues.push(Upvalue { index, is_local });
        self.function.upvalue_count += 1;
        self.function.upvalue_count - 1
    }

    fn parse_variable(&mut self, message: &str) -> Option<SmolStr> {
        self.consume(Type::Identifier, message);

        self.declare_variable();
        if self.scope_depth > 0 {
            return None;
        }

        Some(self.previous().lexeme)
    }

    fn declare_variable(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        for local in self.locals.iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }
            if self.previous().lexeme == local.name.lexeme {
                self.error("Variable with same name already declared in this scope.");
                break;
            }
        }

        self.add_local(self.previous());
    }

    fn define_variable(&mut self, global: Option<SmolStr>) {
        if self.scope_depth == 0 {
            self.emit_opcode(OpCode::DefineGlobal(global.unwrap()));
        } else {
            self.mark_initialized()
        }
    }

    fn argument_list(&mut self) -> UInt {
        let mut arg_count = 0;
        if !self.parser().check(Type::RightParen) {
            loop {
                self.expression();
                arg_count += 1;
                if !self.parser_mut().match_next(Type::Comma) {
                    break;
                }
            }
        }

        self.consume(Type::RightParen, "Expected ')' after arguments.");
        arg_count
    }

    fn add_local(&mut self, name: Token) {
        self.locals.push(Local {
            name,
            depth: self.scope_depth,
            initialized: false,
            captured: false,
        });
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals.last_mut().unwrap().initialized = true;
    }

    fn emit_opcode(&mut self, code: OpCode) {
        let line = self.previous().line;
        self.current_chunk_mut()
            .code
            .push(OpCodeLine { code, line })
    }

    fn emit_opcodes(&mut self, code1: OpCode, code2: OpCode) {
        self.emit_opcode(code1);
        self.emit_opcode(code2);
    }

    fn emit_jump(&mut self, code: OpCode) -> usize {
        self.emit_opcode(code);
        self.current_chunk().code.len() - 1
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().code.len() - offset - 1;

        self.current_chunk_mut().code[offset].code = match self.current_chunk().code[offset].code {
            OpCode::Jump(_) => OpCode::Jump(jump as UInt),
            OpCode::JumpIfFalse(_) => OpCode::JumpIfFalse(jump as UInt),
            _ => panic!("Jump was tried to be patched, opcode was not a jump!"),
        }
    }

    fn emit_loop(&mut self, start: usize) {
        let jump = self.current_chunk().code.len() - start + 1;
        self.emit_opcode(OpCode::Loop(jump as UInt));
    }

    fn emit_return(&mut self) {
        if self.function_type == FunctionType::Initializer {
            self.emit_opcode(OpCode::GetLocal(0));
        } else {
            self.emit_opcode(OpCode::Constant(Constant::Nil));
        }
        self.emit_opcode(OpCode::Return);
    }

    fn end_compiliation(&mut self) -> MutRc<Function> {
        self.emit_return();
        Rc::new(RefCell::new(mem::replace(
            &mut self.function,
            Self::new_function(None, 0),
        )))
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while !self.locals.is_empty()
            && self.locals.last().expect("Empty locals?").depth > self.scope_depth
        {
            let op = if self.locals.last().unwrap().captured {
                OpCode::HoistUpvalue
            } else {
                OpCode::Pop
            };
            self.emit_opcode(op);
            self.locals.pop();
        }
    }

    fn get_rule(t_type: Type) -> &'static ParseRule {
        &RULES[t_type.to_usize()]
    }

    fn current(&self) -> Token {
        self.parser_mut().current.clone()
    }

    fn previous(&self) -> Token {
        self.parser_mut().previous.clone()
    }

    fn current_chunk(&self) -> &Chunk {
        &self.function.chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    fn new_function(name: Option<SmolStr>, arity: UInt) -> Function {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }

    fn consume(&mut self, t_type: Type, message: &str) {
        self.parser_mut().consume(t_type, message)
    }

    fn error(&self, message: &str) {
        self.parser_mut().error(message)
    }

    fn parser_mut(&self) -> RefMut<Parser> {
        self.parser.borrow_mut()
    }

    fn parser(&self) -> Ref<Parser> {
        self.parser.borrow()
    }

    pub fn new(code: &str) -> Compiler {
        Compiler {
            parser: Parser::new(code),

            function: Self::new_function(None, 0),
            function_type: FunctionType::Script,

            locals: vec![Local {
                name: Token::generic_token(Type::Identifier),
                depth: 0,
                initialized: false,
                captured: false,
            }],
            scope_depth: 0,
            upvalues: SmallVec::new(),

            enclosing: None,
            class_stack: Rc::new(RefCell::new(Vec::with_capacity(2))),
        }
    }
}

plain_enum_mod! {this, Precedence {
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

#[derive(Debug)]
struct Local {
    name: Token,
    depth: usize,
    initialized: bool,
    captured: bool,
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler, bool)>,
    infix: Option<fn(&mut Compiler, bool)>,
    precedence: Precedence,
}

impl ParseRule {
    const fn new(precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix: None,
            infix: None,
            precedence,
        }
    }

    const fn new_both(
        prefix: fn(&mut Compiler, bool),
        infix: Option<fn(&mut Compiler, bool)>,
        precedence: Precedence,
    ) -> ParseRule {
        ParseRule {
            prefix: Some(prefix),
            infix,
            precedence,
        }
    }

    const fn new_infix(infix: fn(&mut Compiler, bool), precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix: None,
            infix: Some(infix),
            precedence,
        }
    }
}

static RULES: [ParseRule; 40] = [
    ParseRule::new_both(
        // LEFT_PAREN
        |compiler, _| compiler.grouping(),
        Some(|compiler, _| compiler.call()),
        Precedence::Call,
    ),
    ParseRule::new(Precedence::None), // RIGHT_PAREN
    ParseRule::new(Precedence::None), // LEFT_BRACE
    ParseRule::new(Precedence::None), // RIGHT_BRACE
    ParseRule::new(Precedence::None), // COMMA
    ParseRule::new_infix(
        |compiler, can_assign| compiler.dot(can_assign),
        Precedence::Call,
    ), // DOT
    ParseRule::new_both(
        // MINUS
        |compiler, _| compiler.unary(),
        Some(|compiler, _| compiler.binary()),
        Precedence::Term,
    ),
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Term), // PLUS
    ParseRule::new(Precedence::None),                                        // SEMICOLON
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Factor), // SLASH
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Factor), // STAR
    ParseRule::new_both(|compiler, _| compiler.unary(), None, Precedence::None), // BANG
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Equality), // BANG_EQUAL
    ParseRule::new(Precedence::None),                                        // EQUAL
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Equality), // EQUAL_EQUAL
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Comparison), // GREATER
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Comparison), // GREATER_EQUAL
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Comparison), // LESS
    ParseRule::new_infix(|compiler, _| compiler.binary(), Precedence::Comparison), // LESS_EQUAL
    ParseRule::new_both(
        // IDENTIFIER
        |compiler, can_assign| compiler.variable(can_assign),
        None,
        Precedence::None,
    ),
    ParseRule::new_both(|compiler, _| compiler.string(), None, Precedence::Term), // STRING
    ParseRule::new_both(|compiler, _| compiler.literal(), None, Precedence::None), // NUMBER
    ParseRule::new_infix(|compiler, _| compiler.and(), Precedence::And),          // AND
    ParseRule::new(Precedence::None),                                             // CLASS
    ParseRule::new(Precedence::None),                                             // ELSE
    ParseRule::new_both(|compiler, _| compiler.literal(), None, Precedence::None), // FALSE
    ParseRule::new(Precedence::None),                                             // FOR
    ParseRule::new(Precedence::None),                                             // FUN
    ParseRule::new(Precedence::None),                                             // IF
    ParseRule::new_both(|compiler, _| compiler.literal(), None, Precedence::None), // NIL
    ParseRule::new_infix(|compiler, _| compiler.or(), Precedence::Or),            // OR
    ParseRule::new(Precedence::None),                                             // PRINT
    ParseRule::new(Precedence::None),                                             // RETURN
    ParseRule::new_both(|compiler, _| compiler.super_(), None, Precedence::None), // SUPER
    ParseRule::new_both(|compiler, _| compiler.this(), None, Precedence::None),   // THIS
    ParseRule::new_both(|compiler, _| compiler.literal(), None, Precedence::None), // TRUE
    ParseRule::new(Precedence::None),                                             // VAR
    ParseRule::new(Precedence::None),                                             // WHILE
    ParseRule::new(Precedence::None),                                             // ERROR
    ParseRule::new(Precedence::None),                                             // EOF
];
