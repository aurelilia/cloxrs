use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Token {
    pub t_type: Type,
    pub lexeme: Rc<String>,
    pub line: usize,
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
