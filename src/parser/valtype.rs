use crate::interpreter::val::Val;
use crate::lexer::token::{Token, TokenType};
use std::fmt;

pub const TYPE_UNINIT: &str = "uninit";
pub const TYPE_ANY: &str = "any";
pub const TYPE_BOOL: &str = "bool";
pub const TYPE_FN: &str = "fn";
pub const TYPE_NUM: &str = "num";
pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_STR: &str = "str";
pub const TYPE_NIL: &str = "nil";
pub const TYPE_VEC: &str = "vec";
pub const TYPE_MAP: &str = "map";
pub const TYPE_STRUCT: &str = "struct";
pub const TYPE_ENUM: &str = "enum";

#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    Uninit,
    Num,
    Int,
    Float,
    Bool,
    Nil,
    Str,
    Vec(Generics),
    Map,
    Fn,
    /// Corresponds to both enum & struct.
    Instance(String),
    Any,
}

impl ValType {
    pub fn try_from_token(token: &Token, generics: Option<Vec<Self>>) -> Option<Self> {
        match token.token_type {
            TokenType::Num => Some(Self::Num),
            TokenType::Int => Some(Self::Int),
            TokenType::Float => Some(Self::Float),
            TokenType::Bool => Some(Self::Bool),
            TokenType::Nil => Some(Self::Nil),
            TokenType::Str => Some(Self::Str),
            TokenType::Vec => {
                let generics = generics.unwrap_or_else(|| vec![Self::Any]);
                Some(Self::Vec(Generics::new(generics)))
            }
            TokenType::Map => Some(Self::Map),
            TokenType::Fn => Some(Self::Fn),
            TokenType::Any => Some(Self::Any),
            TokenType::Identifier => Some(Self::Instance(token.lexeme.clone())),
            _ => None,
        }
    }

    pub fn try_from_val(val: &Val) -> Option<Self> {
        match val {
            Val::Uninit => Some(Self::Uninit),
            Val::Float(_) => Some(Self::Float),
            Val::Int(_) => Some(Self::Int),
            Val::Bool(_) => Some(Self::Bool),
            Val::Nil => Some(Self::Nil),
            Val::Str(_) => Some(Self::Str),
            Val::Callable(_) => Some(Self::Fn),
            Val::StructInstance(i) => Some(Self::Instance(i.borrow_mut().struct_name.clone())),
            Val::EnumValue(e, _, _) => Some(Self::Instance(e.clone())),
            Val::VecInstance(v) => Some(Self::Vec(Generics::new(vec![v
                .borrow_mut()
                .val_type
                .clone()]))),
            _ => None,
        }
    }

    pub fn conforms(&self, val: &Val) -> bool {
        match (self, val) {
            (_, Val::Uninit) => true,
            (Self::Any, _) => true,
            (Self::Nil, Val::Nil) => true,
            (Self::Bool, Val::Bool(_)) => true,
            (Self::Fn, Val::Callable(_)) => true,
            (Self::Num, Val::Float(_)) => true,
            (Self::Num, Val::Int(_)) => true,
            (Self::Int, Val::Int(_)) => true,
            (Self::Float, Val::Float(_)) => true,
            (Self::Float, Val::Int(_)) => true,
            (Self::Str, Val::Str(_)) => true,
            (Self::Instance(s), Val::StructInstance(i)) => {
                let i = i.borrow();
                i.struct_name == *s || i.impls.contains(s)
            }
            (Self::Instance(s), Val::EnumValue(e, _, _)) => s == e,
            (Self::Vec(g), Val::VecInstance(v)) => {
                let v_g_type = g.types.first().unwrap();
                let vi_g_type = v.borrow_mut().val_type.clone();

                *v_g_type == vi_g_type
            }
            _ => false,
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Uninit => write!(f, "{}", TYPE_UNINIT),
            Self::Any => write!(f, "{}", TYPE_ANY),
            Self::Bool => write!(f, "{}", TYPE_BOOL),
            Self::Fn => write!(f, "{}", TYPE_FN),
            Self::Num => write!(f, "{}", TYPE_NUM),
            Self::Int => write!(f, "{}", TYPE_INT),
            Self::Float => write!(f, "{}", TYPE_FLOAT),
            Self::Str => write!(f, "{}", TYPE_STR),
            Self::Nil => write!(f, "{}", TYPE_NIL),
            Self::Vec(g) => write!(f, "{}<{}>", TYPE_VEC, g.types.first().unwrap()),
            Self::Map => write!(f, "{}", TYPE_MAP),
            Self::Instance(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    types: Vec<ValType>,
}

impl Generics {
    pub fn new(types: Vec<ValType>) -> Self {
        Self { types }
    }
}
