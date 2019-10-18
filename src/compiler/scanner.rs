use super::token::{Token, Type};
use std::rc::Rc;

/// A scanner is an iterator that turns Lox source code into [Token]s.
pub struct Scanner {
    /// The chars of the source
    chars: Vec<char>,
    /// The start position of the token currently being scanned
    start: usize,
    /// The current position of the scan
    current: usize,
    /// The line of the current position
    line: usize,
}

impl Scanner {
    pub fn scan_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.start = self.current;

        let ch = self.advance()?;
        Some(match ch {
            // Single-char
            '(' => self.make_token(Type::LeftParen),
            ')' => self.make_token(Type::RightParen),
            '{' => self.make_token(Type::LeftBrace),
            '}' => self.make_token(Type::RightBrace),
            ';' => self.make_token(Type::Semicolon),
            ',' => self.make_token(Type::Comma),
            '.' => self.make_token(Type::Dot),
            '-' => self.make_token(Type::Minus),
            '+' => self.make_token(Type::Plus),
            '*' => self.make_token(Type::Star),
            '/' => self.make_token(Type::Slash),

            // Double-char
            '!' => self.check_double_token('=', Type::BangEqual, Type::Bang),
            '=' => self.check_double_token('=', Type::EqualEqual, Type::Equal),
            '<' => self.check_double_token('=', Type::LessEqual, Type::Less),
            '>' => self.check_double_token('=', Type::GreaterEqual, Type::Greater),

            // Literals
            '"' => self.string(),
            _ if ch.is_ascii_digit() => self.number(),

            // Identifiers/Keywords
            _ if (ch.is_alphabetic() || ch == '_') => self.identifier(),

            _ => self.error_token("Unexpected symbol."),
        })
    }

    fn check_double_token(&mut self, next: char, matched: Type, not_matched: Type) -> Token {
        let token = if self.match_next(next) {
            matched
        } else {
            not_matched
        };
        self.make_token(token)
    }

    /// Creates an identifier or keyword token.
    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let mut token = self.make_token(Type::Identifier);

        token.t_type = match &token.lexeme[..] {
            "and" => Type::And,
            "class" => Type::Class,
            "else" => Type::Else,
            "false" => Type::False,
            "for" => Type::For,
            "fun" => Type::Fun,
            "if" => Type::If,
            "nil" => Type::Nil,
            "or" => Type::Or,
            "print" => Type::Print,
            "return" => Type::Return,
            "super" => Type::Super,
            "this" => Type::This,
            "true" => Type::True,
            "var" => Type::Var,
            "while" => Type::While,

            _ => Type::Identifier,
        };

        token
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_twice().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(Type::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_token("Unterminated string!")
        } else {
            self.start += 1;
            let token = self.make_token(Type::String);
            self.advance();
            token
        }
    }

    /// Creates a token based on the current position of self.start and self.current
    fn make_token(&mut self, t_type: Type) -> Token {
        Token {
            t_type,
            lexeme: Rc::new(self.chars[(self.start)..(self.current)].iter().collect()),
            line: self.line,
        }
    }

    /// Creates a ScanError token with the given message at the current location
    fn error_token(&mut self, message: &'static str) -> Token {
        Token {
            t_type: Type::Error,
            lexeme: Rc::new(message.to_string()),
            line: self.line,
        }
    }


    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }

                '\n' => {
                    self.line += 1;
                    self.advance();
                }

                '/' => {
                    if self.peek_twice() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }

                _ => return,
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek() == '\0'
    }

    fn match_next(&mut self, expected: char) -> bool {
        let matches = self.peek() == expected;
        if matches {
            self.advance();
        }
        matches
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.chars.get(self.current - 1).map(|f| *f)
    }

    fn peek(&self) -> char {
        **self.chars.get(self.current).get_or_insert(&'\0')
    }

    fn peek_twice(&self) -> char {
        self.chars[self.current + 1]
    }

    pub fn new(source: String) -> Scanner {
        let chars: Vec<char> = source.chars().collect();
        Scanner {
            chars,
            start: 0,
            current: 0,
            line: 1,
        }
    }
}
