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
    Fn(FnType),
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
            TokenType::Any => Some(Self::Any),
            TokenType::Identifier => Some(Self::Instance(token.lexeme.clone())),
            _ => None,
        }
    }

    pub fn try_from_val(val: &Val) -> Option<Self> {
        Some(match val {
            Val::Uninit => Self::Uninit,
            Val::Float(_) => Self::Float,
            Val::Int(_) => Self::Int,
            Val::Bool(_) => Self::Bool,
            Val::Nil => Self::Nil,
            Val::Str(_) => Self::Str,
            Val::StructInstance(i) => Self::Instance(i.borrow_mut().struct_name.clone()),
            Val::EnumValue(e, _, _) => Self::Instance(e.clone()),
            Val::VecInstance(v) => Self::Vec(Generics::new(vec![v.borrow_mut().val_type.clone()])),
            Val::Callable(c) => Self::Fn(FnType::new(
                None,
                c.param_types.clone(),
                Box::new(c.ret_type.clone()),
            )),
            Val::Any(_) => Self::Any,
            _ => return None,
        })
    }

    pub fn conforms(&self, val: &Val) -> bool {
        match (self, val) {
            (_, Val::Uninit) => true,
            (Self::Any, _) => true,
            (Self::Nil, Val::Nil) => true,
            (Self::Bool, Val::Bool(_)) => true,
            (Self::Fn(fn_type), Val::Callable(call)) => {
                *fn_type.ret_type == call.ret_type && *fn_type.param_types == call.param_types
            }
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
            Self::Fn(fn_type) => write!(f, "{}", fn_type),
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

#[derive(Debug, Clone)]
pub struct FnType {
    token: Option<Token>,
    param_types: Vec<ValType>,
    ret_type: Box<ValType>,
}

impl FnType {
    const TYPE: &'static str = TYPE_FN;

    pub fn new(token: Option<Token>, param_types: Vec<ValType>, ret_type: Box<ValType>) -> Self {
        Self {
            token,
            param_types,
            ret_type,
        }
    }

    pub fn get_type(param_types: &[ValType], ret_type: &ValType) -> String {
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
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Self::get_type(&self.param_types, &self.ret_type))
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

    #[test]
    fn test_try_from_token() {
        let int = Token::new(
            TokenType::Int,
            String::from("100"),
            String::from("100"),
            (0, 0),
        );
        let float = Token::new(
            TokenType::Float,
            String::from("10.1"),
            String::from("10.1"),
            (0, 0),
        );
        let string = Token::new(
            TokenType::Str,
            String::from("string"),
            String::from("string"),
            (0, 0),
        );
        let nil = Token::new(
            TokenType::Nil,
            String::from("nil"),
            String::from("nil"),
            (0, 0),
        );
        let boolean = Token::new(
            TokenType::Bool,
            String::from("true"),
            String::from("true"),
            (0, 0),
        );

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

    #[test]
    fn test_try_from_val() {
        let int = Val::Int(100);
        let float = Val::Float(10.1);
        let string = Val::Str(str::to_string("string"));
        let nil = Val::Nil;
        let boolean = Val::Bool(true);

        assert_eq!(ValType::try_from_val(&int).unwrap(), ValType::Int);
        assert_eq!(ValType::try_from_val(&float).unwrap(), ValType::Float);
        assert_eq!(ValType::try_from_val(&string).unwrap(), ValType::Str);
        assert_eq!(ValType::try_from_val(&nil).unwrap(), ValType::Nil);
        assert_eq!(ValType::try_from_val(&boolean).unwrap(), ValType::Bool);
    }

    #[test]
    fn test_conforms() {
        let int = Val::Int(100);
        let float = Val::Float(10.1);
        let string = Val::Str(str::to_string("string"));
        let nil = Val::Nil;
        let boolean = Val::Bool(true);

        assert!(ValType::Int.conforms(&int));
        assert!(!ValType::Int.conforms(&float));
        assert!(!ValType::Int.conforms(&string));

        assert!(ValType::Float.conforms(&float));
        assert!(ValType::Float.conforms(&int));
        assert!(!ValType::Float.conforms(&string));

        assert!(ValType::Str.conforms(&string));
        assert!(!ValType::Str.conforms(&int));
        assert!(!ValType::Str.conforms(&boolean));

        assert!(ValType::Any.conforms(&string));
        assert!(ValType::Any.conforms(&nil));
        assert!(ValType::Any.conforms(&boolean));
        assert!(ValType::Any.conforms(&int));
        assert!(ValType::Any.conforms(&float));
    }
}
