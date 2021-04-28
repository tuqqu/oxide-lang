use std::{error, fmt};

use super::token::Pos;

#[derive(Debug)]
pub enum LexerError {
    /// Error when a comment has no closing delimiter.
    UnclosedComment(Pos),
    /// Unknown character when scanning the source.
    UnknownCharacter(Pos, char),
    /// String has no closing quote delimiter.
    UnterminatedString(Pos),
}

impl error::Error for LexerError {}

impl LexerError {
    fn msg(&self) -> String {
        match self {
            Self::UnclosedComment(_) => String::from("Unclosed comment"),
            Self::UnknownCharacter(_, ch) => format!("Unknown character \"{}\"", ch),
            Self::UnterminatedString(_) => String::from("Unterminated string"),
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnclosedComment(pos)
            | Self::UnknownCharacter(pos, _)
            | Self::UnterminatedString(pos) => write!(f, "Error: {} at {}", self.msg(), pos),
        }
    }
}
