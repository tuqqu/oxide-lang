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

    pub fn from_token(token: &Self, lexeme: String) -> Self {
        Self {
            token_type: token.token_type,
            lexeme,
            literal: token.literal.clone(),
            pos: token.pos,
        }
    }
}

impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme && self.token_type == other.token_type
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

    As,

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_eq() {
        let a = Token::new(TokenType::Identifier, String::from("lexeme"), String::from(""), (0, 0));
        let b = Token::new(TokenType::Identifier, String::from("lexeme"), String::from(""), (10, 10));
        let c = Token::new(TokenType::Identifier, String::from("another_lexeme"), String::from(""), (0, 0));
        let d = Token::new(TokenType::String, String::from("lexeme"), String::from(""), (0, 0));

        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(a, d);
    }

    #[test]
    fn test_from_token() {
        let a = Token::new(TokenType::Identifier, String::from("lexeme"), String::from(""), (0, 0));
        let b = Token::from_token(&a, String::from("another_lexeme"));

        assert_ne!(a, b);

        assert_eq!(a.pos, b.pos);
        assert_eq!(a.token_type, b.token_type);
        assert_eq!(a.literal, b.literal);

        assert_eq!(b.lexeme, "another_lexeme");
    }
}
