use std::fmt;

pub use self::token_type::TokenType;

mod token_type;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub pos: Pos,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, pos: Pos) -> Self {
        Self {
            token_type,
            lexeme,
            pos,
        }
    }

    pub fn from_token(token: &Self, lexeme: String) -> Self {
        Self {
            token_type: token.token_type,
            lexeme,
            pos: token.pos,
        }
    }
}

impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme && self.token_type == other.token_type
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Pos(pub usize, pub usize);

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}:{}]", self.0, self.1)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_eq() {
        let a = Token::new(TokenType::Identifier, String::from("lexeme"), Pos(0, 0));
        let b = Token::new(TokenType::Identifier, String::from("lexeme"), Pos(10, 10));
        let c = Token::new(
            TokenType::Identifier,
            String::from("another_lexeme"),
            Pos(0, 0),
        );
        let d = Token::new(TokenType::String, String::from("lexeme"), Pos(0, 0));

        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(a, d);
    }

    #[test]
    fn test_from_token() {
        let a = Token::new(TokenType::Identifier, String::from("lexeme"), Pos(0, 0));
        let b = Token::from_token(&a, String::from("another_lexeme"));

        assert_ne!(a, b);
        assert_eq!(a.pos, b.pos);
        assert_eq!(a.token_type, b.token_type);
        assert_eq!(b.lexeme, "another_lexeme");
    }
}
