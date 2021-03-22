use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use crate::parser::expr::{
    Lambda, StructDecl, ValType, TYPE_BOOL, TYPE_FLOAT, TYPE_FUNC, TYPE_INT, TYPE_NIL,
    TYPE_STR, TYPE_STRUCT, TYPE_STRUCT_INSTANCE, TYPE_UNINIT,
};

use super::env::Env;
use super::Interpreter;
use super::Result;

use crate::interpreter::RuntimeError;
use crate::lexer::token::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Val {
    Uninit,
    Nil,
    Bool(bool),
    Str(String),
    Int(isize),
    Float(f64),
    Callable(Callable),
    Struct(StructCallable),
    StructInstance(StructInstance),
}

#[derive(Debug)]
pub enum StmtVal {
    None,
    Break,
    Continue,
    Return(Val),
}

pub type Func = Arc<dyn Fn(&mut Interpreter, &Vec<Val>) -> Result<Val>>;
pub type Constructor = Arc<dyn Fn(&mut Interpreter, &Vec<(Token, Val)>) -> Result<Val>>;

pub struct Callable {
    pub arity: usize,
    pub call: Box<Func>,
}

pub struct StructCallable {
    pub arity: usize,
    pub call: Box<Constructor>,
}

#[derive(Debug, Clone)]
pub struct StructInstance {
    pub props: HashMap<String, (Val, ValType)>,
    pub fns: HashMap<String, Lambda>,
    pub struct_name: String,
}

#[derive(Clone)]
pub struct Function {
    pub lambda: Lambda,
    pub env: Rc<RefCell<Env>>,
}

impl Function {
    pub fn new(lambda: Lambda, env: Rc<RefCell<Env>>) -> Self {
        Self { lambda, env }
    }

    pub fn param_size(&self) -> usize {
        self.lambda.params.len()
    }
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Callable")
            .field("arity", &self.arity)
            .finish()
    }
}

impl fmt::Debug for StructCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StructCallable")
            .field("arity", &self.arity)
            .finish()
    }
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            call: self.call.clone(),
        }
    }
}

impl Clone for StructCallable {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            call: self.call.clone(),
        }
    }
}

impl Callable {
    pub fn new(arity: usize, call: Func) -> Box<Self> {
        Box::new(Self {
            arity,
            call: Box::new(call),
        })
    }
}

impl StructCallable {
    pub fn new(arity: usize, call: Constructor) -> Box<Self> {
        Box::new(Self {
            arity,
            call: Box::new(call),
        })
    }
}

pub enum PropFuncVal {
    Prop(Val),
    Func(Lambda),
}

impl StructInstance {
    pub fn new(struct_: StructDecl) -> Self {
        let mut props: HashMap<String, (Val, ValType)> = HashMap::new();
        for prop in struct_.props {
            // we can be sure that v_type is always present
            props.insert(prop.name.lexeme, (Val::Uninit, prop.v_type.unwrap()));
        }

        let mut fns: HashMap<String, Lambda> = HashMap::new();
        for fun in struct_.fns {
            fns.insert(fun.name.lexeme, fun.lambda);
        }

        Self {
            props,
            fns,
            struct_name: struct_.name.lexeme,
        }
    }

    pub fn get_prop(&self, name: &Token) -> Result<PropFuncVal> {
        if !self.props.contains_key(&name.lexeme) {
            if !self.fns.contains_key(&name.lexeme) {
                Err(RuntimeError::from_token(
                    name.clone(),
                    format!("No struct property with name \"{}\"", &name.lexeme),
                ))
            } else {
                let func = self.fns.get(&name.lexeme).unwrap();

                Ok(PropFuncVal::Func(func.clone()))
            }
        } else {
            match self.props.get(&name.lexeme).unwrap() {
                (Val::Uninit, _) => Err(RuntimeError::from_token(
                    name.clone(),
                    format!(
                        "Property \"{}\" has not yet been initialized.",
                        &name.lexeme
                    ),
                )),
                (val, _) => Ok(PropFuncVal::Prop(val.clone())),
            }
        }
    }

    pub fn set_prop(&mut self, name: &Token, val: Val) -> Result<()> {
        if !self.props.contains_key(&name.lexeme) {
            Err(RuntimeError::from_token(
                name.clone(),
                format!("No struct property with name \"{}\"", &name.lexeme),
            ))
        } else {
            let (_, v_type) = self.props.get(&name.lexeme).unwrap();
            let v_type = v_type.clone();
            if v_type.conforms(&val) {
                self.props
                    .insert(name.lexeme.clone(), (val, v_type));

                Ok(())
            } else {
                Err(RuntimeError::from_token(
                    name.clone(),
                    format!(
                        "Trying to assign to a variable of type \"{}\" value pof type \"{}\"",
                        v_type.to_string(),
                        val.get_type()
                    ),
                ))
            }
        }
    }
}

impl Val {
    pub fn to_string(&self) -> String {
        use Val::*;

        match self {
            Uninit => TYPE_UNINIT.to_string(),
            Nil => TYPE_NIL.to_string(),
            Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Str(s) => s.clone(),
            Int(n) => n.to_string(),
            Float(n) => n.to_string(),
            Callable(_f) => TYPE_FUNC.to_string(),
            Struct(_c) => "struct".to_string(),
            StructInstance(i) => {
                let mut props = vec![];
                for (prop, (val, _val_t)) in &i.props {
                    props.push(format!("{}: {}", prop, val.to_string()));
                }

                format!("[struct] {} {{ {} }}, ", i.struct_name, props.join(", "))
            }
        }
    }

    pub fn get_type(&self) -> String {
        use Val::*;

        match self {
            Uninit => TYPE_UNINIT,
            Nil => TYPE_NIL,
            Bool(_bool) => TYPE_BOOL,
            Str(_str) => TYPE_STR,
            Int(_isize) => TYPE_INT,
            Float(_f64) => TYPE_FLOAT,
            Callable(_f) => TYPE_FUNC,
            Struct(_c) => TYPE_STRUCT,
            StructInstance(_i) => TYPE_STRUCT_INSTANCE,
        }
        .to_string()
    }
}
