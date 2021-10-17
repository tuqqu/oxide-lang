use std::{fmt, ops};

use crate::lexer::{Token, TokenType};

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
pub const TYPE_TRAIT: &str = "trait";
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
    Fn(FnType),
    /// Corresponds to both enum & struct.
    Instance(String),
    Any,
    Union(Vec<Self>),
}

impl ValType {
    pub(crate) fn try_from_token(token: &Token, generics: Option<Vec<Self>>) -> Option<Self> {
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
            TokenType::Any => Some(Self::Any),
            TokenType::Identifier => Some(Self::Instance(token.lexeme.clone())),
            _ => None,
        }
    }
}

impl ops::Add for ValType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Union(mut v_a), Self::Union(mut v_b)) => {
                v_a.append(&mut v_b);
                Self::Union(v_a)
            }
            (Self::Union(mut v_a), rhs) => {
                v_a.push(rhs);
                Self::Union(v_a)
            }
            (lhs, Self::Union(mut v_a)) => {
                v_a.push(lhs);
                Self::Union(v_a)
            }
            (lhs, rhs) => Self::Union(vec![lhs, rhs]),
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Uninit => write!(f, "{}", TYPE_UNINIT),
            Self::Any => write!(f, "{}", TYPE_ANY),
            Self::Bool => write!(f, "{}", TYPE_BOOL),
            Self::Fn(fn_type) => write!(f, "{}", fn_type),
            Self::Num => write!(f, "{}", TYPE_NUM),
            Self::Int => write!(f, "{}", TYPE_INT),
            Self::Float => write!(f, "{}", TYPE_FLOAT),
            Self::Str => write!(f, "{}", TYPE_STR),
            Self::Nil => write!(f, "{}", TYPE_NIL),
            Self::Vec(g) => write!(f, "{}<{}>", TYPE_VEC, g.types.first().unwrap()),
            Self::Map => write!(f, "{}", TYPE_MAP),
            Self::Instance(s) => write!(f, "{}", s),
            Self::Union(v) => write!(
                f,
                "{}",
                v.iter()
                    .map(|vt| vt.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
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

    pub fn types(&self) -> &[ValType] {
        &self.types
    }
}

#[derive(Debug, Clone)]
pub struct FnType {
    param_types: Vec<ValType>,
    ret_type: Box<ValType>,
}

impl FnType {
    const TYPE: &'static str = TYPE_FN;

    pub fn construct_type(param_types: &[ValType], ret_type: &ValType) -> String {
        format!(
            "{}({}){}",
            Self::TYPE,
            param_types
                .iter()
                .map(|vt| vt.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            if *ret_type != ValType::Nil {
                format!(" -> {}", ret_type)
            } else {
                String::from("")
            }
        )
    }

    pub fn new(param_types: Vec<ValType>, ret_type: Box<ValType>) -> Self {
        Self {
            param_types,
            ret_type,
        }
    }

    pub fn param_types(&self) -> &[ValType] {
        &self.param_types
    }

    pub fn ret_type(&self) -> &ValType {
        &self.ret_type
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            Self::construct_type(&self.param_types, &self.ret_type)
        )
    }
}

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        self.ret_type == other.ret_type && self.param_types == other.param_types
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Pos;

    #[test]
    fn test_try_from_token() {
        let int = Token::new(TokenType::Int, String::from("100"), Pos(0, 0));
        let float = Token::new(TokenType::Float, String::from("10.1"), Pos(0, 0));
        let string = Token::new(TokenType::Str, String::from("string"), Pos(0, 0));
        let nil = Token::new(TokenType::Nil, String::from("nil"), Pos(0, 0));
        let boolean = Token::new(TokenType::Bool, String::from("true"), Pos(0, 0));

        assert_eq!(ValType::try_from_token(&int, None).unwrap(), ValType::Int);
        assert_eq!(
            ValType::try_from_token(&float, None).unwrap(),
            ValType::Float
        );
        assert_eq!(
            ValType::try_from_token(&string, None).unwrap(),
            ValType::Str
        );
        assert_eq!(ValType::try_from_token(&nil, None).unwrap(), ValType::Nil);
        assert_eq!(
            ValType::try_from_token(&boolean, None).unwrap(),
            ValType::Bool
        );
    }
}
