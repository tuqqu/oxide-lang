use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::result;

use oxide_parser::Token;

use super::val::Val;
use crate::env_val::{
    Constant, Enum, EnumValue, EnvVal, Function, Impl, ResolvableName, Struct, Trait, Type,
    Variable,
};
use crate::error::RuntimeError;
use crate::val::StructInstance;

#[derive(Clone, Debug)]
pub(crate) struct Env {
    vals: HashMap<String, Rc<RefCell<EnvVal>>>,
    self_: Option<Rc<RefCell<StructInstance>>>,
    static_bind: Option<String>,
    impls: HashMap<String, Vec<Impl>>,
    enclosing: Option<Rc<RefCell<Self>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) type EnvResult<T> = result::Result<T, RuntimeError>;

impl Env {
    pub(crate) fn new() -> Self {
        Self::from_enclosing(None)
    }

    pub(crate) fn with_enclosing(enclosing: Rc<RefCell<Self>>) -> Self {
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
    pub(crate) fn define_from(&mut self, val: EnvVal) -> EnvResult<()> {
        use EnvVal::*;

        match val {
            Trait(_t) => {}
            Variable(v) => self.define_variable(v),
            Constant(c) => self.define_constant(c)?,
            Function(f) => self.define_function(f)?,
            Enum(e) => self.define_enum(e),
            EnumValue(e) => self.define_enum_value(e),
            Struct(s) => self.define_struct(s),
            Type(t) => self.define_type(t),
        };

        Ok(())
    }

    pub(crate) fn define_variable(&mut self, var: Variable) {
        self.vals.insert(
            var.name.clone(),
            Rc::new(RefCell::new(EnvVal::Variable(var))),
        );
    }

    pub(crate) fn define_enum(&mut self, enum_: Enum) {
        self.vals.insert(
            enum_.name().to_string(),
            Rc::new(RefCell::new(EnvVal::Enum(enum_))),
        );
    }

    pub(crate) fn define_enum_value(&mut self, enum_value: EnumValue) {
        self.vals.insert(
            enum_value.get_name(),
            Rc::new(RefCell::new(EnvVal::EnumValue(enum_value))),
        );
    }

    pub(crate) fn has_definition(&self, name: &str) -> bool {
        self.vals.contains_key(name)
    }

    pub(crate) fn define_struct(&mut self, struct_: Struct) {
        self.vals.insert(
            struct_.name().to_string(),
            Rc::new(RefCell::new(EnvVal::Struct(struct_))),
        );
    }

    pub(crate) fn define_impl(&mut self, impl_: Impl) -> EnvResult<()> {
        self.impls
            .entry(impl_.impl_name().to_string())
            .or_default()
            .push(impl_);

        Ok(())
    }

    pub(crate) fn define_trait(&mut self, trait_: Trait) {
        self.vals.insert(
            trait_.name().to_string(),
            Rc::new(RefCell::new(EnvVal::Trait(trait_))),
        );
    }

    pub(crate) fn define_type(&mut self, type_: Type) {
        self.vals.insert(
            type_.name().to_string(),
            Rc::new(RefCell::new(EnvVal::Type(type_))),
        );
    }

    pub(crate) fn define_self(&mut self, self_: Rc<RefCell<StructInstance>>) {
        self.self_ = Some(self_);
    }

    pub(crate) fn define_static_bind(&mut self, static_bind: String) {
        self.static_bind = Some(static_bind);
    }

    pub(crate) fn define_constant(&mut self, constant: Constant) -> EnvResult<()> {
        self.check_defined(&constant)?;
        self.vals.insert(
            constant.resolve_name(),
            Rc::new(RefCell::new(EnvVal::Constant(constant))),
        );

        Ok(())
    }

    pub(crate) fn define_function(&mut self, func: Function) -> EnvResult<()> {
        self.check_defined(&func)?;
        self.vals.insert(
            func.resolve_name(),
            Rc::new(RefCell::new(EnvVal::Function(func))),
        );

        Ok(())
    }

    pub(crate) fn get(&mut self, name: &str) -> EnvResult<Rc<RefCell<EnvVal>>> {
        if self.vals.contains_key(name) {
            let val = self.vals.get(name);

            return match val {
                Some(val) => Ok(val.clone()),
                None => unreachable!(),
            };
        }

        if self.enclosing.is_some() {
            let val = self.enclosing.as_ref().unwrap().borrow_mut().get(name);

            return val;
        }

        Err(RuntimeError::Definition(
            None,
            format!("Trying to access an undefined value \"{}\"", name),
        ))
    }

    pub(crate) fn get_impls(&mut self, name: &Token) -> Option<Vec<Impl>> {
        if self.impls.contains_key(&name.lexeme) {
            let impl_ = self.impls.get(&name.lexeme);

            return impl_.cloned();
        }

        if self.enclosing.is_some() {
            return self
                .enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .get_impls(name);
        }

        None
    }

    pub(crate) fn get_self(&self) -> Option<Rc<RefCell<StructInstance>>> {
        if self.self_.is_some() {
            return self.self_.clone();
        }

        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().borrow_mut().get_self();
        }

        None
    }

    pub(crate) fn get_static_bind(&self) -> Option<String> {
        if self.static_bind.is_some() {
            return self.static_bind.clone();
        }

        if self.enclosing.is_some() {
            return self.enclosing.as_ref().unwrap().borrow().get_static_bind();
        }

        None
    }

    pub(crate) fn assign(&mut self, name: Token, val: &Val) -> EnvResult<()> {
        #[allow(clippy::map_entry)]
        if self.vals.contains_key(&name.lexeme) {
            let env_val = self.get(&name.lexeme)?;
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

        Err(RuntimeError::Runtime(
            name.clone(),
            format!("Trying to assign to an undefined value \"{}\"", name.lexeme),
        ))
    }

    fn check_defined(&self, name: &impl ResolvableName) -> EnvResult<()> {
        if self.vals.contains_key(&name.resolve_name()) {
            return Err(RuntimeError::Definition(
                Some(name.name().clone()),
                format!("Name \"{}\" is already in use", name.resolve_name()),
            ));
        }

        Ok(())
    }
}
