use std::{error, fmt};

use crate::lexer::token::token_type::TokenType;
use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub enum ParseError {
    /// Generic error at an unexpected token.
    UnexpectedToken(Token, Option<TokenType>, String),
    /// Unexpected expression.
    UnexpectedExpr(Token, String),
    /// Expected a block statement.
    BlockExpected(Token),
    /// Unexpected expression.
    InstanceContext(Token, String),
    /// Unexpected expression.
    TooManyParams(Token, usize),
    /// Generic parameter number does not match.
    UnmatchedGenericParamsNumber(Token, usize, usize),
    /// Expected a type.
    TypeExpected(Token, String),
    /// Cannot recognise a given type.
    UnknownType(Token),
    /// Token can be used only inside a loop.
    MustBeInsideLoop(Token),
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(unexpected, Some(expected), msg) if msg.is_empty() => write!(
                f,
                "Error: Unexpected token \"{}\", expected \"{}\" at {}",
                unexpected.lexeme, expected, unexpected.pos
            ),
            Self::UnexpectedToken(unexpected, None, msg) if msg.is_empty() => write!(
                f,
                "Error: Unexpected token \"{}\" at {}",
                unexpected.lexeme, unexpected.pos
            ),
            Self::UnexpectedToken(token, _, msg) => {
                write!(f, "Error \"{}\": {} at {}", token.lexeme, msg, token.pos)
            }
            Self::UnexpectedExpr(token, msg) => {
                write!(f, "Error \"{}\": {} at {}", token.lexeme, msg, token.pos)
            }
            Self::BlockExpected(token) => write!(
                f,
                "Error \"{}\": Block statement expected at {}",
                token.lexeme, token.pos
            ),
            Self::InstanceContext(token, msg) => {
                write!(f, "Error \"{}\": {} at {}", token.lexeme, msg, token.pos)
            }
            Self::TooManyParams(token, size) => write!(
                f,
                "Error \"{}\": Function cannot have more than {} arguments at {}",
                token.lexeme, size, token.pos
            ),
            Self::UnmatchedGenericParamsNumber(token, exp_size, act_size) => write!(
                f,
                "Error \"{}\": Expected {} generics parameters, got {} at {}",
                token.lexeme, exp_size, act_size, token.pos
            ),
            Self::TypeExpected(token, msg) => {
                write!(f, "Error \"{}\": {} at {}", token.lexeme, msg, token.pos)
            }
            Self::UnknownType(token) => write!(
                f,
                "Error \"{}\": Unknown type at {}",
                token.lexeme, token.pos
            ),
            Self::MustBeInsideLoop(token) => write!(
                f,
                "Error \"{}\": Must be inside a loop to use \"{}\" at {}",
                token.lexeme, token.lexeme, token.pos
            ),
        }
    }
}
