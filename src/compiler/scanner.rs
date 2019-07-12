use super::token::{Token, Type};

pub struct Scanner<'a> {
    source: &'a str,
    chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(Type::EOF);
        }
        let ch = self.advance();

        if ch.is_ascii_alphabetic() || ch == '_' {
            return self.identifier();
        }
        if ch.is_ascii_digit() {
            return self.number();
        }

        match ch {
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

            // Double-char (TODO: Find a way to shrink this to a oneliner...)
            '!' => {
                let token = if self.match_next('=') {
                    Type::BangEqual
                } else {
                    Type::Bang
                };
                self.make_token(token)
            }
            '=' => {
                let token = if self.match_next('=') {
                    Type::EqualEqual
                } else {
                    Type::Equal
                };
                self.make_token(token)
            }
            '<' => {
                let token = if self.match_next('=') {
                    Type::LessEqual
                } else {
                    Type::Less
                };
                self.make_token(token)
            }
            '>' => {
                let token = if self.match_next('=') {
                    Type::GreaterEqual
                } else {
                    Type::Greater
                };
                self.make_token(token)
            }

            // Literals
            '"' => self.string(),

            _ => self.error_token("Unexpected symbol."),
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> Type {
        match self.chars[self.start] {
            'a' => self.check_identifier_keyword(1, &['n', 'd', ' '], Type::And),
            'c' => self.check_identifier_keyword(1, &['l', 'a', 's', 's', ' '], Type::Class),
            'e' => self.check_identifier_keyword(1, &['l', 's', 'e', ' '], Type::Else),
            'i' => self.check_identifier_keyword(1, &['f', ' '], Type::If),
            'n' => self.check_identifier_keyword(1, &['i', 'l', ' '], Type::Nil),
            'o' => self.check_identifier_keyword(1, &['r', ' '], Type::Or),
            'p' => self.check_identifier_keyword(1, &['r', 'i', 'n', 't', ' '], Type::Print),
            'r' => self.check_identifier_keyword(1, &['e', 't', 'u', 'r', 'n', ' '], Type::Return),
            's' => self.check_identifier_keyword(1, &['u', 'p', 'e', 'r', ' '], Type::Super),
            'v' => self.check_identifier_keyword(1, &['a', 'r', ' '], Type::Var),
            'w' => self.check_identifier_keyword(1, &['h', 'i', 'l', 'e', ' '], Type::While),

            'f' => match self.chars[self.start + 1] {
                'a' => self.check_identifier_keyword(2, &['l', 's', 'e', ' '], Type::False),
                'o' => self.check_identifier_keyword(2, &['r', ' '], Type::For),
                'u' => self.check_identifier_keyword(2, &['n', ' '], Type::Fun),
                _ => Type::Identifier,
            },

            't' => match self.chars[self.start + 1] {
                'h' => self.check_identifier_keyword(2, &['i', 's', ' '], Type::This),
                'r' => self.check_identifier_keyword(2, &['u', 'e', ' '], Type::True),
                _ => Type::Identifier,
            },

            _ => Type::Identifier,
        }
    }

    fn check_identifier_keyword(&self, start: usize, pattern: &[char], t_type: Type) -> Type {
        // Loop all chars in the pattern; if one does not match it is NOT the keyword
        for ch in 0..pattern.len() {
            if self.chars[self.start + start + ch] != pattern[ch] {
                return Type::Identifier;
            }
        }
        
        // All matched! It is the keyword after all
        t_type
    }

    fn number(&mut self) -> Token<'a> {
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

    fn string(&mut self) -> Token<'a> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_token("Unterminated string!")
        } else {
            self.advance();
            self.make_token(Type::String)
        }
    }

    fn make_token(&mut self, t_type: Type) -> Token<'a> {
        Token {
            t_type,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&mut self, message: &'a str) -> Token<'a> {
        Token {
            t_type: Type::Error,
            lexeme: message,
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
        if self.peek() == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars[self.current - 1]
    }

    fn peek(&self) -> char {
        **self.chars.get(self.current).get_or_insert(&'\0')
    }

    fn peek_twice(&self) -> char {
        self.chars[self.current + 1]
    }

    pub fn new(source: &'a str) -> Scanner {
        let chars: Vec<char> = source.chars().collect();
        Scanner {
            source,
            chars,
            start: 0,
            current: 0,
            line: 1,
        }
    }
}
