pub struct Token<'t> {
    pub t_type: Type,
    pub lexeme: &'t str,
    pub line: usize
}

pub enum Type {
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

    Error, EOF
}