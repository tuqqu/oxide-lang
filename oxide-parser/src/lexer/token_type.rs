use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    FatArrow,
    Comma,
    Dot,
    DotDot,
    DotDotEqual,

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

    As,

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

    Loop,
    While,
    For,
    In,

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
    Mod,
    Type,

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

impl TokenType {
    pub fn value(&self) -> &'static str {
        match self {
            Self::Colon => ":",
            Self::ColonColon => "::",
            Self::Semicolon => ";",
            Self::Arrow => "->",
            Self::FatArrow => "=>",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::DotDot => "..",
            Self::DotDotEqual => "..=",

            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftCurlyBrace => "{",
            Self::RightCurlyBrace => "}",
            Self::LeftBracket => "]",
            Self::RightBracket => "[",

            Self::Minus => "-",
            Self::Plus => "+",
            Self::Slash => "/",
            Self::Modulus => "%",
            Self::Asterisk => "*",

            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",

            Self::Bang => "!",
            Self::BangEqual => "!=",
            Self::Equal => "=",
            Self::EqualEqual => "==",

            Self::PlusEqual => "+=",
            Self::MinusEqual => "-=",
            Self::AsteriskEqual => "*=",
            Self::SlashEqual => "/=",
            Self::ModulusEqual => "%=",

            Self::LogicAnd => "&&",
            Self::LogicOr => "||",

            Self::BitwiseAnd => "&",
            Self::BitwiseOr => "|",
            Self::BitwiseXor => "^",

            Self::BitwiseAndEqual => "&=",
            Self::BitwiseOrEqual => "|=",
            Self::BitwiseXorEqual => "^=",

            Self::As => "as",

            Self::Num => "num",
            Self::Int => "int",
            Self::Float => "float",
            Self::Str => "str",
            Self::Bool => "bool",
            Self::Nil => "nil",
            Self::Vec => "vec",
            Self::Map => "map",
            Self::Any => "any",

            Self::Loop => "loop",
            Self::While => "while",
            Self::For => "for",
            Self::In => "in",

            Self::Match => "match",
            Self::If => "if",
            Self::Else => "else",

            Self::Let => "let",
            Self::Mut => "mut",
            Self::Const => "const",

            Self::Enum => "enum",
            Self::Struct => "struct",
            Self::Fn => "fn",
            Self::Impl => "impl",
            Self::Trait => "trait",
            Self::Mod => "mod",
            Self::Type => "type",

            Self::Pub => "pub",

            Self::Return => "return",
            Self::Continue => "continue",
            Self::Break => "break",

            Self::False => "false",
            Self::True => "true",

            Self::Self_ => "self",
            Self::SelfStatic => "Self",

            _ => "",
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}
