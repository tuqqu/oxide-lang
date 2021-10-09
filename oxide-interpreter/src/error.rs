use std::{error, fmt};

use oxide_parser::Token;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    /// Error in definitions of items.
    Definition(Option<Token>, String),
    /// Error related to wrong operation usage.
    Operator(Token, String),
    /// Program structure error.
    Script(Option<Token>, String),
    /// The most generic runtime error.
    Runtime(Token, String),
    /// Error related to type system.
    Type(Option<Token>, String),
}

impl RuntimeError {
    fn name(&self) -> &'static str {
        match self {
            Self::Definition(..) => "Definition error",
            Self::Script(..) => "Script error",
            Self::Type(..) => "Type error",
            Self::Operator(..) => "Operator error",
            Self::Runtime(..) => "Runtime error",
        }
    }
}

impl error::Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Definition(Some(token), msg)
            | Self::Script(Some(token), msg)
            | Self::Type(Some(token), msg)
            | Self::Operator(token, msg)
            | Self::Runtime(token, msg) => {
                write!(
                    f,
                    "{} \"{}\": {} at {}",
                    self.name(),
                    token.lexeme,
                    msg,
                    token.pos
                )
            }
            Self::Definition(None, msg)
            | Self::Script(None, msg)
            | Self::Type(None, msg) => write!(f, "{}: {}", self.name(), msg),
        }
    }
}
