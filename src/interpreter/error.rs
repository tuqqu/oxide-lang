use std::{error, fmt};

use crate::lexer::token::Token;

#[derive(Debug)]
pub enum RuntimeError {
    /// Error in definitions of items.
    DefinitionError(Option<Token>, String),
    /// Error related to wrong operation usage.
    OperatorError(Token, String),
    /// Program structure error.
    ScriptError(Option<Token>, String),
    /// The most generic runtime error.
    RuntimeError(Token, String),
    /// Error related to type system.
    TypeError(Option<Token>, String),
}

impl error::Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DefinitionError(Some(token), msg)
            | Self::ScriptError(Some(token), msg)
            | Self::TypeError(Some(token), msg)
            | Self::OperatorError(token, msg)
            | Self::RuntimeError(token, msg) => {
                write!(f, "Error \"{}\": {} at {}", token.lexeme, msg, token.pos)
            }
            Self::DefinitionError(None, msg)
            | Self::ScriptError(None, msg)
            | Self::TypeError(None, msg) => write!(f, "Error: {}", msg),
        }
    }
}
