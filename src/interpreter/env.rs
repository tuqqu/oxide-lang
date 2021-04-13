use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::interpreter::Result;
use crate::interpreter::RuntimeError;
use crate::lexer::token::{Pos, Token};
use crate::parser::expr::{ConstDecl, FnDecl};

use super::val::Val;
use crate::interpreter::val::StructInstance;
use crate::parser::valtype::ValType;

static COUNTER: AtomicUsize = AtomicUsize::new(1);

pub fn internal_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Clone, Debug)]
pub struct Env {
    pub vals: HashMap<String, Rc<RefCell<EnvVal>>>,
    pub self_: Option<Rc<RefCell<StructInstance>>>,
    pub static_bind: Option<String>,
    pub impls: HashMap<String, Impl>,
    pub enclosing: Option<Rc<RefCell<Self>>>,
}

#[derive(Clone, Debug)]
pub enum EnvVal {
    NoValue,
    Variable(Variable),
    Constant(Constant),
    Function(Function),
    Enum(Enum),
    EnumValue(EnumValue),
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
    pub name: Token,
    pub val: Val,
    pub for_target: Option<(Token, bool)>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: usize,
    pub name: Token,
    pub val: Val,
    pub for_target: Option<(Token, bool)>,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub id: usize,
    pub name: Token,
    pub val: Val,
}

#[derive(Clone, Debug)]
pub struct EnumValue {
    pub id: usize,
    pub name: Token,
    pub val: Val,
    pub for_enum: Token,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: usize,
    pub name: String,
    pub val: Val,
}

/// The only env value, which is not wrapped in `EnvValue` enum value,
/// because we store it separately to lookup called functions & etc.
#[derive(Clone, Debug)]
pub struct Impl {
    pub id: usize,
    pub for_struct: String,
    pub methods: Vec<(FnDecl, bool)>,
    pub fns: Vec<(FnDecl, bool)>,
    pub consts: Vec<(ConstDecl, bool)>,
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
    pub fn with_struct(name: Token, val: Val, for_struct: (Token, bool)) -> Self {
        Self::new(name, val, Some(for_struct))
    }

    pub fn without_struct(name: Token, val: Val) -> Self {
        Self::new(name, val, None)
    }

    fn new(name: Token, val: Val, for_struct: Option<(Token, bool)>) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
            for_target: for_struct,
        }
    }

    pub fn get_name(&self) -> String {
        if let Some((for_struct, _)) = &self.for_target {
            construct_static_name(&for_struct.lexeme, &self.name.lexeme)
        } else {
            self.name.lexeme.clone()
        }
    }
}

impl Function {
    pub fn with_struct(name: Token, val: Val, for_struct: (Token, bool)) -> Self {
        Self::new(name, val, Some(for_struct))
    }

    pub fn without_struct(name: Token, val: Val) -> Self {
        Self::new(name, val, None)
    }

    pub fn new(name: Token, val: Val, for_struct: Option<(Token, bool)>) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
            for_target: for_struct,
        }
    }

    pub fn get_name(&self) -> String {
        if let Some((for_struct, _)) = &self.for_target {
            construct_static_name(&for_struct.lexeme, &self.name.lexeme)
        } else {
            self.name.lexeme.clone()
        }
    }
}

impl Enum {
    pub fn new(name: Token, val: Val) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
        }
    }
}

impl EnumValue {
    pub fn new(name: Token, val: Val, for_enum: Token) -> Self {
        Self {
            id: internal_id(),
            name,
            val,
            for_enum,
        }
    }

    pub fn get_name(&self) -> String {
        construct_static_name(&self.for_enum.lexeme, &self.name.lexeme)
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

impl Impl {
    pub fn new(
        for_struct: String,
        methods: Vec<(FnDecl, bool)>,
        fns: Vec<(FnDecl, bool)>,
        consts: Vec<(ConstDecl, bool)>,
    ) -> Self {
        Self {
            id: internal_id(),
            for_struct,
            methods,
            fns,
            consts,
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
                    if let Some(v_type) = v_type {
                        v.v_type = v_type;
                        v.val = val;
                        Ok(())
                    } else {
                        Err(RuntimeError::from_token(
                            name,
                            format!(
                                "Cannot infer variable type from value of type \"{}\"",
                                val.get_type()
                            ),
                        ))
                    }
                } else if !v.v_type.conforms(&val) {
                    Err(RuntimeError::from_token(
                        name,
                        format!(
                            "Trying to assign to a variable of type \"{}\" value of type \"{}\"",
                            v.v_type,
                            val.get_type()
                        ),
                    ))
                } else if v.mutable {
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
            EnumValue(_) | Enum(_) | Struct(_) => Err(RuntimeError::from_token(
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
            impls: HashMap::new(),
            self_: None,
            static_bind: None,
            enclosing,
        }
    }

    #[allow(dead_code)]
    pub fn define_from(&mut self, val: EnvVal) -> Result<()> {
        use EnvVal::*;

        match val {
            NoValue => {}
            Variable(v) => self.define_variable(v),
            Constant(c) => self.define_constant(c)?,
            Function(f) => self.define_function(f)?,
            Enum(e) => self.define_enum(e),
            EnumValue(e) => self.define_enum_value(e),
            Struct(s) => self.define_struct(s),
        };

        Ok(())
    }

    pub fn define_variable(&mut self, var: Variable) {
        self.vals.insert(
            var.name.clone(),
            Rc::new(RefCell::new(EnvVal::Variable(var))),
        );
    }

    pub fn define_enum(&mut self, enum_: Enum) {
        self.vals.insert(
            enum_.name.lexeme.clone(),
            Rc::new(RefCell::new(EnvVal::Enum(enum_))),
        );
    }

    pub fn define_enum_value(&mut self, enum_value: EnumValue) {
        self.vals.insert(
            enum_value.get_name(),
            Rc::new(RefCell::new(EnvVal::EnumValue(enum_value))),
        );
    }

    pub fn has_definition(&self, name: &str) -> bool {
        self.vals.contains_key(name)
    }

    pub fn define_struct(&mut self, struct_: Struct) {
        self.vals.insert(
            struct_.name.clone(),
            Rc::new(RefCell::new(EnvVal::Struct(struct_))),
        );
    }

    pub fn define_impl(&mut self, impl_: Impl) -> Result<()> {
        self.impls.insert(impl_.for_struct.clone(), impl_);

        Ok(())
    }

    pub fn define_self(&mut self, self_: Rc<RefCell<StructInstance>>) -> Result<()> {
        self.self_ = Some(self_);

        Ok(())
    }

    pub fn define_static_bind(&mut self, static_bind: String) -> Result<()> {
        self.static_bind = Some(static_bind);

        Ok(())
    }

    pub fn define_constant(&mut self, constant: Constant) -> Result<()> {
        if self.vals.contains_key(&constant.get_name()) {
            return Err(RuntimeError::from_token(
                constant.name.clone(),
                format!("Trying to redefine constant \"{}\"", constant.get_name()),
            ));
        }

        self.vals.insert(
            constant.get_name(),
            Rc::new(RefCell::new(EnvVal::Constant(constant))),
        );

        Ok(())
    }

    pub fn define_function(&mut self, func: Function) -> Result<()> {
        if self.vals.contains_key(&func.get_name()) {
            return Err(RuntimeError::from_token(
                func.name.clone(),
                format!("Name \"{}\" is already in use", func.get_name()),
            ));
        }

        self.vals.insert(
            func.get_name(),
            Rc::new(RefCell::new(EnvVal::Function(func))),
        );

        Ok(())
    }

    // FIXME: remove this function
    pub fn get(&mut self, name: Token) -> Result<Rc<RefCell<EnvVal>>> {
        self.get_by_str(&name.lexeme, name.pos)
    }

    pub fn get_by_str(&mut self, name: &str, pos: Pos) -> Result<Rc<RefCell<EnvVal>>> {
        if self.vals.contains_key(name) {
            let val = self.vals.get(name);

            return match val {
                Some(val) => Ok(val.clone()),
                None => {
                    // unreachable
                    Ok(Rc::new(RefCell::new(EnvVal::NoValue)))
                }
            };
        }

        if self.enclosing.is_some() {
            let val = self
                .enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .get_by_str(name, pos);

            return val;
        }

        Err(RuntimeError::from_pos(
            pos,
            format!("Trying to access undefined value \"{}\"", name),
        ))
    }

    pub fn get_impl(&mut self, name: &Token) -> Option<Impl> {
        if self.impls.contains_key(&name.lexeme) {
            let impl_ = self.impls.get(&name.lexeme);

            return match impl_ {
                Some(impl_) => Some(impl_.clone()),
                None => None,
            };
        }

        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().borrow_mut().get_impl(name);
        }

        None
    }

    pub fn get_self(&self) -> Option<Rc<RefCell<StructInstance>>> {
        if self.self_.is_some() {
            return self.self_.clone();
        }

        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().borrow_mut().get_self();
        }

        None
    }

    /// FIXME: return a whole struct reference here, not only the name
    pub fn get_static_bind(&self) -> Option<String> {
        if self.static_bind.is_some() {
            return self.static_bind.clone();
        }

        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().borrow().get_static_bind();
        }

        None
    }

    pub fn assign(&mut self, name: Token, val: &Val) -> Result<()> {
        if self.vals.contains_key(&name.lexeme) {
            let env_val = self.get(name.clone())?;
            env_val
                .borrow_mut()
                .try_to_assign(val.clone(), name.clone())?;

            self.vals.insert(name.lexeme, env_val);

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

        Err(RuntimeError::from_token(
            name.clone(),
            format!("Trying to assign to an undefined value \"{}\"", name.lexeme),
        ))
    }
}

pub fn construct_static_name(struct_name: &str, static_field: &str) -> String {
    format!("{}::{}", struct_name, static_field)
}
