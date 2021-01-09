use crate::interner::{self, StrId};

#[derive(Debug, Clone)]
pub struct Token {
    pub t_type: Type,
    pub lexeme: StrId,
    pub line: usize,
}

impl Token {
    pub fn generic_token(token: Type) -> Token {
        Token {
            t_type: token,
            lexeme: interner::intern(""),
            line: 0,
        }
    }

    pub fn generic_ident(text: &str) -> Token {
        Token {
            t_type: Type::Identifier,
            lexeme: interner::intern(text),
            line: 0,
        }
    }
}

plain_enum_mod! {this, Type {
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot,Minus,Plus,
    Semicolon, Slash, Star,

    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, String, Number,

    And, Class, Else, False,
    For, Fun, If, Nil, Or,
    Print, Return, Super, This,
    True, Var, While,

    Error, EOF,
}}
