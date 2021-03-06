use std::{error, fmt};

use crate::lexer::token::Token;

#[derive(Debug, Clone)]
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

impl RuntimeError {
    fn name(&self) -> &'static str {
        match self {
            Self::DefinitionError(..) => "Definition error",
            Self::ScriptError(..) => "Script error",
            Self::TypeError(..) => "Type error",
            Self::OperatorError(..) => "Operator error",
            Self::RuntimeError(..) => "Runtime error",
        }
    }
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
                write!(
                    f,
                    "{} \"{}\": {} at {}",
                    self.name(),
                    token.lexeme,
                    msg,
                    token.pos
                )
            }
            Self::DefinitionError(None, msg)
            | Self::ScriptError(None, msg)
            | Self::TypeError(None, msg) => write!(f, "{}: {}", self.name(), msg),
        }
    }
}
