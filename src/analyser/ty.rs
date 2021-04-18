use crate::parser::valtype::ValType;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Bool,
    Nil,
    Function(Box<FunctionType>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub return_type: Type,
    pub param_types: Vec<Type>
}

impl Type {
    pub fn from(val_type: &ValType) -> Self {
        match val_type {
            ValType::Int => Self::Int,
            ValType::Str => Self::Str,
            ValType::Nil => Self::Nil,
            ValType::Bool => Self::Bool,
            _ => Self::Nil,
        }
    }
}

impl FunctionType {
    pub fn new(return_type: Type, param_types: Vec<Type>) -> Self {
        Self {
            return_type,
            param_types
        }
    }
}
