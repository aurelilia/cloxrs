#[derive(Debug, Clone, Copy)]
pub struct Token<'t> {
    pub t_type: Type,
    pub lexeme: &'t str,
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
