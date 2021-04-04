use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use crate::parser::expr::{
    Lambda, StructDecl, ValType, TYPE_BOOL, TYPE_FLOAT, TYPE_FUNC, TYPE_INT, TYPE_NIL, TYPE_STR,
    TYPE_STRUCT, TYPE_STRUCT_INSTANCE, TYPE_UNINIT, TYPE_VEC,
};

use super::env::Env;
use super::Interpreter;
use super::Result;

use crate::interpreter::env::{internal_id, Impl};
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
    StructInstance(Rc<RefCell<StructInstance>>),
    VecInstance(Rc<RefCell<VecInstance>>),
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
    pub id: usize,
    pub props: HashMap<String, (Val, ValType, bool)>,
    pub fns: HashMap<String, (Lambda, Rc<RefCell<Self>>, bool)>,
    pub struct_name: String,
}

#[derive(Debug, Clone)]
pub struct VecInstance {
    pub id: usize,
    pub fns: HashMap<String, Lambda>,
    pub vals: Vec<Val>,
    pub val_type: ValType,
}

#[derive(Clone)]
pub struct Function {
    pub id: usize,
    pub lambda: Lambda,
    pub env: Rc<RefCell<Env>>,
}

impl Function {
    pub fn new(lambda: Lambda, env: Rc<RefCell<Env>>) -> Self {
        Self {
            id: internal_id(),
            lambda,
            env,
        }
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
    Func((Lambda, Rc<RefCell<StructInstance>>, bool)),
}

impl StructInstance {
    pub fn new(struct_: StructDecl, impl_: Option<Impl>) -> Rc<RefCell<Self>> {
        let mut props: HashMap<String, (Val, ValType, bool)> = HashMap::new();
        for (prop, public) in struct_.props {
            // we can be sure that v_type is always present
            props.insert(
                prop.name.lexeme,
                (Val::Uninit, prop.v_type.unwrap(), public),
            );
        }

        let instance = Self {
            id: internal_id(),
            props,
            fns: HashMap::new(),
            struct_name: struct_.name.lexeme,
        };

        let self_ = Rc::new(RefCell::new(instance));

        if let Some(impl_) = impl_ {
            let mut fns = HashMap::new();
            for (fun, pub_) in impl_.fns {
                fns.insert(fun.name.lexeme, (fun.lambda, self_.clone(), pub_));
            }

            self_.borrow_mut().fns = fns;
        }

        // FIXME: add constant handling from impl

        self_
    }

    pub fn get_prop(&self, name: &Token, public_access: bool) -> Result<PropFuncVal> {
        if !self.props.contains_key(&name.lexeme) {
            if !self.fns.contains_key(&name.lexeme) {
                Err(RuntimeError::from_token(
                    name.clone(),
                    format!("No struct property with name \"{}\"", &name.lexeme),
                ))
            } else {
                let func = self.fns.get(&name.lexeme).unwrap();
                if Self::can_access(func.2, public_access) {
                    Ok(PropFuncVal::Func(func.clone()))
                } else {
                    Err(RuntimeError::from_token(
                        name.clone(),
                        format!("Cannot access private method \"{}\"", &name.lexeme),
                    ))
                }
            }
        } else {
            match self.props.get(&name.lexeme).unwrap() {
                (Val::Uninit, _, pub_) => {
                    let msg = if Self::can_access(*pub_, public_access) {
                        format!("Cannot access private property \"{}\"", &name.lexeme)
                    } else {
                        format!(
                            "Property \"{}\" has not yet been initialized.",
                            &name.lexeme
                        )
                    };

                    Err(RuntimeError::from_token(name.clone(), msg))
                }
                (val, _, pub_) => {
                    if Self::can_access(*pub_, public_access) {
                        Ok(PropFuncVal::Prop(val.clone()))
                    } else {
                        Err(RuntimeError::from_token(
                            name.clone(),
                            format!("Cannot access private property \"{}\"", &name.lexeme),
                        ))
                    }
                }
            }
        }
    }

    pub fn set_prop(&mut self, name: &Token, val: Val, public_access: bool) -> Result<()> {
        if !self.props.contains_key(&name.lexeme) {
            Err(RuntimeError::from_token(
                name.clone(),
                format!("No struct property with name \"{}\"", &name.lexeme),
            ))
        } else {
            let (_, v_type, public) = self.props.get(&name.lexeme).unwrap();

            if !*public && public_access {
                return Err(RuntimeError::from_token(
                    name.clone(),
                    format!("Cannot access private property \"{}\"", &name.lexeme),
                ));
            }

            let public = *public;
            let v_type = v_type.clone();
            if v_type.conforms(&val) {
                self.props
                    .insert(name.lexeme.clone(), (val, v_type, public));

                Ok(())
            } else {
                Err(RuntimeError::from_token(
                    name.clone(),
                    format!(
                        "Trying to assign to a variable of type \"{}\" value of type \"{}\"",
                        v_type,
                        val.get_type()
                    ),
                ))
            }
        }
    }

    fn can_access(prop_pub: bool, access_pub: bool) -> bool {
        if prop_pub {
            true
        } else {
            !access_pub
        }
    }
}

impl VecInstance {
    pub fn new(vals: Vec<Val>, val_type: ValType) -> Self {
        Self {
            id: internal_id(),
            fns: HashMap::new(),
            vals,
            val_type,
        }
    }

    pub fn get(&self, i: usize) -> Result<Val> {
        let val = if let Some(val) = self.vals.get(i) {
            val.clone()
        } else {
            Val::Uninit
        };

        Ok(val)
    }

    pub fn set(&mut self, i: usize, val: Val) -> Result<()> {
        self.vals[i] = val;

        Ok(())
    }

    pub fn len(&self) -> usize {
        self.vals.len()
    }

    pub fn get_method(name: &Token, vec: Rc<RefCell<VecInstance>>) -> Result<Val> {
        const POP: &str = "pop";
        const PUSH: &str = "push";
        const LEN: &str = "len";

        let callable = match name.lexeme.as_str() {
            POP => Val::Callable(*Callable::new(
                0,
                Arc::new(move |_inter, _args| {
                    let poped = vec.borrow_mut().vals.pop().unwrap_or(Val::Uninit);

                    Ok(poped)
                }),
            )),
            PUSH => Val::Callable(*Callable::new(
                1,
                Arc::new(move |_inter, args| {
                    for arg in args {
                        if !vec.borrow_mut().val_type.conforms(arg) {
                            return Err(RuntimeError::new(
                                0,
                                format!(
                                    "Cannot push value of type \"{}\" to a vector of type \"{}\"",
                                    ValType::try_from_val(arg).unwrap(), // FIXME: may be an unsuccessful transformation
                                    vec.borrow_mut().val_type
                                ),
                            ));
                        }
                        vec.borrow_mut().vals.push(arg.clone());
                    }

                    Ok(Val::VecInstance(vec.clone()))
                }),
            )),
            LEN => Val::Callable(*Callable::new(
                0,
                Arc::new(move |_inter, _args| Ok(Val::Int(vec.borrow_mut().len() as isize))),
            )),
            _ => {
                return Err(RuntimeError::from_token(
                    name.clone(),
                    format!("Unknown vec method \"{}\"", name.lexeme),
                ))
            }
        };

        Ok(callable)
    }
}

impl Val {
    pub const FLOAT_ERROR_MARGIN: f64 = f64::EPSILON;

    pub fn equal(a: &Self, b: &Self) -> bool {
        use Val::*;

        match (a, b) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => (a - b).abs() < Self::FLOAT_ERROR_MARGIN,
            (Float(a), Int(b)) => (*a - *b as f64).abs() < Self::FLOAT_ERROR_MARGIN,
            (Int(a), Float(b)) => (*a as f64 - *b).abs() < Self::FLOAT_ERROR_MARGIN,
            _ => false,
            // FIXME: add struct comparisons
            // FIXME: add vec comparisons
        }
    }

    pub fn get_type(&self) -> String {
        use Val::*;

        match self {
            Uninit => TYPE_UNINIT.to_string(),
            Nil => TYPE_NIL.to_string(),
            Bool(_bool) => TYPE_BOOL.to_string(),
            Str(_str) => TYPE_STR.to_string(),
            Int(_isize) => TYPE_INT.to_string(),
            Float(_f64) => TYPE_FLOAT.to_string(),
            Callable(_f) => TYPE_FUNC.to_string(),
            Struct(_c) => TYPE_STRUCT.to_string(),
            StructInstance(_i) => TYPE_STRUCT_INSTANCE.to_string(),
            VecInstance(v) => format!("{}<{}>", TYPE_VEC, v.borrow_mut().val_type),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Val::*;
        match self {
            Uninit => write!(f, "{}", TYPE_UNINIT),
            Nil => write!(f, "{}", TYPE_NIL),
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Str(s) => write!(f, "{}", s),
            Int(n) => write!(f, "{}", n),
            Float(n) => write!(f, "{}", n),
            Callable(_f) => write!(f, "{}", TYPE_FUNC),
            Struct(_c) => write!(f, "struct"),
            StructInstance(i) => {
                let mut props = vec![];
                for (prop, (val, _val_t, _pub)) in &i.borrow_mut().props {
                    props.push(format!("{}: {}", prop, val));
                }

                write!(
                    f,
                    "[struct] {} {{ {} }}",
                    i.borrow_mut().struct_name,
                    props.join(", ")
                )
            }
            VecInstance(v) => {
                let mut vals = vec![];
                for val in &v.borrow_mut().vals {
                    vals.push(val.to_string());
                }

                write!(f, "[vec] [{}]", vals.join(", "))
            }
        }
    }
}
