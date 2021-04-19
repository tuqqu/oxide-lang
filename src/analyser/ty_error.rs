use super::*;
use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub enum TypeError {
    MismatchedTypes(TypeMismatch),
    AmbiguousTypes,
    CannotResolveType,
    InvalidBinaryOperation(Type, Type, Token),
    UndefinedVariable(Token),
    ReturnOutSideFunction,
    ReturnTypeMismatch(TypeMismatch),
    NotAFunction,
    ArityMismatch,
    FunctionParamsArgsTypeMismatch,
    ConditionNotBool,
    CannotMutateVariable(CannotMutateVariable),
}

#[derive(Debug, Clone)]
pub struct CannotMutateVariable {
    at: Token,
    due_to: Token,
}

#[derive(Debug, Clone)]
pub struct TypeMismatch {
    expected: Type,
    due_to: Token,
    got: Type,
    at: Token,
}

impl TypeMismatch {
    pub fn new(expected: Type, due_to: Token, got: Type, at: Token) -> Self {
        Self {
            expected,
            due_to,
            got,
            at,
        }
    }
    fn destruct(&self) -> (&Type, &Token, &Type, &Token) {
        (&self.expected, &self.due_to, &self.got, &self.at)
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedTypes(tm) => Self::write_tm(tm, formatter),
            Self::ReturnTypeMismatch(tm) => Self::write_tm(tm, formatter),
            _ => formatter.write_fmt(format_args!("{:?}", self)),
        }
    }
}

impl TypeError {
    fn write_tm(tm: &TypeMismatch, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (expected, due_to, got, at) = tm.destruct();
        formatter.write_fmt(format_args!(
            "Expected type: `{:?}` at `{:?}`, but got type: `{:?}` at `{:?}`",
            expected, due_to, got, at
        ))
    }
}

impl TypeError {
    pub fn cannot_mutate_variable(at: Token, due_to: Token) -> Self {
        Self::CannotMutateVariable(CannotMutateVariable { at, due_to })
    }

    pub fn return_type_mismatch(expected: Type, due_to: Token, got: Type, at: Token) -> Self {
        Self::ReturnTypeMismatch(TypeMismatch::new(expected, due_to, got, at))
    }

    pub fn type_mismatch(expected: Type, due_to: Token, got: Type, at: Token) -> Self {
        Self::MismatchedTypes(TypeMismatch::new(expected, due_to, got, at))
    }
}
