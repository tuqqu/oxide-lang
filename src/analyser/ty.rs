use super::TypeError;
use crate::parser::valtype::ValType;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Bool,
    Nil,
    Function(Box<FunctionType>),
    StructType(Rc<StructType>),
    Instance(Rc<StructType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub return_type: Type,
    pub param_types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub name: String,
    pub instance_properties: HashMap<String, (Type, bool)>,
}

impl Type {
    pub fn new_function(return_type: Self, param_types: Vec<Self>) -> Self {
        Self::Function(Box::new(FunctionType {
            return_type,
            param_types,
        }))
    }

    pub fn new_struct_type(
        name: String,
        instance_properties: HashMap<String, (Type, bool)>,
    ) -> Self {
        Self::StructType(Rc::new(StructType {
            name,
            instance_properties,
        }))
    }

    pub fn from(val_type: &ValType) -> Self {
        match val_type {
            ValType::Int => Self::Int,
            ValType::Str => Self::Str,
            ValType::Nil => Self::Nil,
            ValType::Bool => Self::Bool,
            _ => Self::Nil,
        }
    }

    pub fn to_struct_instance(&self) -> Result<&StructType, TypeError> {
        if let Self::Instance(struct_type) = self {
            Ok(struct_type)
        } else {
            Err(TypeError::NotAnInstance)
        }
    }

    pub fn to_struct_type(&self) -> Result<Rc<StructType>, TypeError> {
        if let Self::StructType(rc) = self {
            Ok(rc.clone())
        } else {
            Err(TypeError::NotAStruct)
        }
    }
}

impl FunctionType {
    pub fn new(return_type: Type, param_types: Vec<Type>) -> Self {
        Self {
            return_type,
            param_types,
        }
    }
}
