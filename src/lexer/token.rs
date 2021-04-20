pub type Pos = (usize, usize);

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: String,
    pub pos: Pos,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: String, pos: Pos) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            pos,
        }
    }
}

impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    FatArrow,
    Comma,
    Dot,

    LeftParen,
    RightParen,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftBracket,
    RightBracket,

    Minus,
    Plus,
    Slash,
    Modulus,
    Asterisk,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    ModulusEqual,

    LogicAnd,
    LogicOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    BitwiseAndEqual,
    BitwiseOrEqual,
    BitwiseXorEqual,

    Identifier,
    String,
    NumberInt,
    NumberFloat,

    Num,
    Int,
    Float,
    Str,
    Bool,
    Nil,
    Vec,
    Map,
    Any,

    While,
    For,
    Loop,

    Match,

    If,
    Else,

    Let,
    Mut,
    Const,

    Enum,
    Struct,
    Fn,
    Impl,
    Trait,

    Pub,

    Return,
    Continue,
    Break,

    False,
    True,

    Self_,
    SelfStatic,

    Eof,
}
