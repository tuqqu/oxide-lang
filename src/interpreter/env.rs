use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::interpreter::Result;
use crate::interpreter::RuntimeError;
use crate::lexer::token::Token;
use crate::parser::expr::{ValType};

use super::val::Val;

static COUNTER: AtomicUsize = AtomicUsize::new(1);

fn internal_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Env {
    pub vals: HashMap<String, EnvVal>,
    pub enclosing: Option<Rc<RefCell<Self>>>,
}

#[derive(Clone, Debug)]
pub enum EnvVal {
    NoValue,
    Variable(Variable),
    Constant(Constant),
    Function(Function),
    Struct(Struct),
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub id: usize,
    pub name: String,
    pub val: Val,
    pub v_type: ValType,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub id: usize,
    pub name: String,
    pub val: Val,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: usize,
    pub name: String,
    pub val: Val,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: usize,
    pub name: String,
    pub val: Val,
}

impl Variable {
    pub fn new(name: String, val: Val, mutable: bool, v_type: ValType) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
            mutable,
            v_type,
        }
    }
}

impl Constant {
    pub fn new(name: String, val: Val) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
        }
    }
}

impl Function {
    pub fn new(name: String, val: Val) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
        }
    }
}

impl Struct {
    pub fn new(name: String, val: Val) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
        }
    }
}

impl EnvVal {
    fn try_to_assign(&mut self, val: Val, name: Token) -> Result<()> {
        use EnvVal::*;

        return match self {
            NoValue => Err(RuntimeError::from_token(
                name,
                "Trying to assign to an immutable value.".to_string(),
            )),
            Constant(_c) => Err(RuntimeError::from_token(
                name,
                "Trying to assign to a constant.".to_string(),
            )),
            Function(_f) => Err(RuntimeError::from_token(
                name,
                "Trying to assign to an immutable value.".to_string(),
            )),
            Variable(v) => {
                if let ValType::Uninit = v.v_type {
                    let v_type = ValType::try_from_val(&val);
                    if v_type.is_none() {
                        Err(RuntimeError::from_token(
                            name,
                            format!(
                                "Cannot infer variable type from value of type \"{}\"",
                                val.get_type()
                            ),
                        ))
                    } else {
                        v.v_type = v_type.unwrap();
                        v.val = val;
                        Ok(())
                    }
                } else if !v.v_type.conforms(&val) {
                    Err(RuntimeError::from_token(
                        name,
                        format!(
                            "Trying to assign to a variable of type \"{}\" value pof type \"{}\"",
                            v.v_type.to_string(),
                            val.get_type()
                        ),
                    ))
                } else {
                    if v.mutable {
                        v.val = val;
                        Ok(())
                    } else if let Val::Uninit = v.val {
                        v.val = val;
                        Ok(())
                    } else {
                        Err(RuntimeError::from_token(
                            name,
                            "Trying to assign to an immutable variable.".to_string(),
                        ))
                    }
                }
            }
            Struct(_f) => Err(RuntimeError::from_token(
                name,
                "Trying to assign to a non-value.".to_string(),
            )),
        };
    }
}

impl Env {
    pub fn new() -> Self {
        Self::from_enclosing(None)
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Self>>) -> Self {
        Self::from_enclosing(Some(enclosing))
    }

    fn from_enclosing(enclosing: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            vals: HashMap::new(),
            enclosing,
        }
    }

    #[allow(dead_code)]
    pub fn define_from(&mut self, val: EnvVal) -> Result<()> {
        use EnvVal::*;

        match val {
            NoValue => Ok(()),
            Variable(v) => {
                self.define_variable(v);
                Ok(())
            }
            Constant(c) => self.define_constant(c),
            Function(f) => self.define_function(f),
            Struct(s) => {
                self.define_struct(s);
                Ok(())
            }
        }
    }

    pub fn define_variable(&mut self, var: Variable) {
        self.vals.insert(var.name.clone(), EnvVal::Variable(var));
    }

    pub fn define_struct(&mut self, struct_: Struct) {
        self.vals
            .insert(struct_.name.clone(), EnvVal::Struct(struct_));
    }

    pub fn define_constant(&mut self, constant: Constant) -> Result<()> {
        if self.vals.contains_key(&constant.name) {
            return Err(RuntimeError::new(
                0,
                format!("Trying to redefine constant \"{}\"", constant.name),
            ));
        }

        self.vals
            .insert(constant.name.clone(), EnvVal::Constant(constant));

        Ok(())
    }

    pub fn define_function(&mut self, func: Function) -> Result<()> {
        if self.vals.contains_key(&func.name) {
            return Err(RuntimeError::new(
                0,
                format!("Trying to redefine function \"{}\"", func.name),
            ));
        }

        self.vals.insert(func.name.clone(), EnvVal::Function(func));

        Ok(())
    }

    pub fn get(&mut self, name: Token) -> Result<EnvVal> {
        if self.vals.contains_key(&name.lexeme) {
            let val = self.vals.get(&name.lexeme);

            return match val {
                Some(val) => Ok(val.clone()),
                None => {
                    // unreachable
                    Ok(EnvVal::NoValue)
                }
            };
        }

        if self.enclosing.is_some() {
            let val = self.enclosing.as_ref().unwrap().borrow_mut().get(name);
            return val;
        }

        Err(RuntimeError::new(
            name.line,
            format!("Trying to access undefined value \"{}\"", name.lexeme),
        ))
    }

    pub fn assign(&mut self, name: Token, val: &Val) -> Result<()> {
        if self.vals.contains_key(&name.lexeme) {
            let env_val = self.get(name.clone())?;
            let mut env_val = env_val.clone();

            env_val.try_to_assign(val.clone(), name.clone())?;

            self.vals.insert(name.lexeme, env_val.clone());

            return Ok(());
        }

        if self.enclosing.is_some() {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign(name, val)?;

            return Ok(());
        }

        Err(RuntimeError::new(
            name.line,
            format!("Trying to assign to an undefined value \"{}\"", name.lexeme),
        ))
    }
}