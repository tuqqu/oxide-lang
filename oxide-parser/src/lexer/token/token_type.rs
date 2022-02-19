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
    pub fn value(&self) -> Option<&'static str> {
        match self {
            Self::Colon => Some(":"),
            Self::ColonColon => Some("::"),
            Self::Semicolon => Some(";"),
            Self::Arrow => Some("->"),
            Self::FatArrow => Some("=>"),
            Self::Comma => Some(","),
            Self::Dot => Some("."),
            Self::DotDot => Some(".."),
            Self::DotDotEqual => Some("..="),

            Self::LeftParen => Some("("),
            Self::RightParen => Some(")"),
            Self::LeftCurlyBrace => Some("{"),
            Self::RightCurlyBrace => Some("}"),
            Self::LeftBracket => Some("]"),
            Self::RightBracket => Some("["),

            Self::Minus => Some("-"),
            Self::Plus => Some("+"),
            Self::Slash => Some("/"),
            Self::Modulus => Some("%"),
            Self::Asterisk => Some("*"),

            Self::Greater => Some(">"),
            Self::GreaterEqual => Some(">="),
            Self::Less => Some("<"),
            Self::LessEqual => Some("<="),

            Self::Bang => Some("!"),
            Self::BangEqual => Some("!="),
            Self::Equal => Some("="),
            Self::EqualEqual => Some("=="),

            Self::PlusEqual => Some("+="),
            Self::MinusEqual => Some("-="),
            Self::AsteriskEqual => Some("*="),
            Self::SlashEqual => Some("/="),
            Self::ModulusEqual => Some("%="),

            Self::LogicAnd => Some("&&"),
            Self::LogicOr => Some("||"),

            Self::BitwiseAnd => Some("&"),
            Self::BitwiseOr => Some("|"),
            Self::BitwiseXor => Some("^"),

            Self::BitwiseAndEqual => Some("&="),
            Self::BitwiseOrEqual => Some("|="),
            Self::BitwiseXorEqual => Some("^="),

            Self::As => Some("as"),

            Self::Identifier => None,
            Self::String => None,
            Self::NumberInt => None,
            Self::NumberFloat => None,

            Self::Num => Some("num"),
            Self::Int => Some("int"),
            Self::Float => Some("float"),
            Self::Str => Some("str"),
            Self::Bool => Some("bool"),
            Self::Nil => Some("nil"),
            Self::Vec => Some("vec"),
            Self::Map => Some("map"),
            Self::Any => Some("any"),

            Self::Loop => Some("loop"),
            Self::While => Some("while"),
            Self::For => Some("for"),
            Self::In => Some("in"),

            Self::Match => Some("match"),
            Self::If => Some("if"),
            Self::Else => Some("else"),

            Self::Let => Some("let"),
            Self::Mut => Some("mut"),
            Self::Const => Some("const"),

            Self::Enum => Some("enum"),
            Self::Struct => Some("struct"),
            Self::Fn => Some("fn"),
            Self::Impl => Some("impl"),
            Self::Trait => Some("trait"),
            Self::Mod => Some("mod"),
            Self::Type => Some("type"),

            Self::Pub => Some("pub"),

            Self::Return => Some("return"),
            Self::Continue => Some("continue"),
            Self::Break => Some("break"),

            Self::False => Some("false"),
            Self::True => Some("true"),

            Self::Self_ => Some("self"),
            Self::SelfStatic => Some("Self"),

            Self::Eof => None,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value().unwrap_or(""))
    }
}
