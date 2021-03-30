#![allow(dead_code)]

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Colon,
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
    Func,
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

    Struct,
    Fn,
    Impl,

    Pub,

    Return,
    Continue,
    Break,

    False,
    True,

    Super,
    Self_,

    Eof,
}
