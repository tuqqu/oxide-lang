use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use oxide_parser::expr::{ConstDecl, FnDecl, FnSignatureDecl};
use oxide_parser::{Token, ValType};

use super::val::Val;
use crate::error::RuntimeError;
use crate::interpreter::InterpretedResult;
use crate::val::{try_vtype_from_val, vtype_conforms_val, StructInstance};

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
    pub(crate) fn define_from(&mut self, val: EnvVal) -> InterpretedResult<()> {
        use EnvVal::*;

        match val {
            NoValue => {}
            Trait(_t) => {}
            Variable(v) => self.define_variable(v),
            Constant(c) => self.define_constant(c)?,
            Function(f) => self.define_function(f)?,
            Enum(e) => self.define_enum(e),
            EnumValue(e) => self.define_enum_value(e),
            Struct(s) => self.define_struct(s),
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
            enum_.name.lexeme.to_string(),
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
            struct_.name.clone(),
            Rc::new(RefCell::new(EnvVal::Struct(struct_))),
        );
    }

    pub(crate) fn define_impl(&mut self, impl_: Impl) -> InterpretedResult<()> {
        self.impls
            .entry(impl_.impl_name.clone())
            .or_default()
            .push(impl_);

        Ok(())
    }

    pub(crate) fn define_trait(&mut self, trait_: Trait) {
        self.vals.insert(
            trait_.name.clone(),
            Rc::new(RefCell::new(EnvVal::Trait(trait_))),
        );
    }

    pub(crate) fn define_self(&mut self, self_: Rc<RefCell<StructInstance>>) {
        self.self_ = Some(self_);
    }

    pub(crate) fn define_static_bind(&mut self, static_bind: String) {
        self.static_bind = Some(static_bind);
    }

    pub(crate) fn define_constant(&mut self, constant: Constant) -> InterpretedResult<()> {
        self.check_defined(&constant)?;
        self.vals.insert(
            constant.resolve_name(),
            Rc::new(RefCell::new(EnvVal::Constant(constant))),
        );

        Ok(())
    }

    pub(crate) fn define_function(&mut self, func: Function) -> InterpretedResult<()> {
        self.check_defined(&func)?;
        self.vals.insert(
            func.resolve_name(),
            Rc::new(RefCell::new(EnvVal::Function(func))),
        );

        Ok(())
    }

    pub(crate) fn get(&mut self, name: &Token) -> InterpretedResult<Rc<RefCell<EnvVal>>> {
        if self.vals.contains_key(&name.lexeme) {
            let val = self.vals.get(&name.lexeme);

            return match val {
                Some(val) => Ok(val.clone()),
                None => {
                    // unreachable
                    Ok(Rc::new(RefCell::new(EnvVal::NoValue)))
                }
            };
        }

        if self.enclosing.is_some() {
            let val = self.enclosing.as_ref().unwrap().borrow_mut().get(name);

            return val;
        }

        Err(RuntimeError::Runtime(
            name.clone(),
            format!("Trying to access an undefined value \"{}\"", name.lexeme),
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

    pub(crate) fn assign(&mut self, name: Token, val: &Val) -> InterpretedResult<()> {
        #[allow(clippy::map_entry)]
        if self.vals.contains_key(&name.lexeme) {
            let env_val = self.get(&name)?;
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

    fn check_defined(&self, name: &impl ResolvableName) -> InterpretedResult<()> {
        if self.vals.contains_key(&name.resolve_name()) {
            return Err(RuntimeError::Definition(
                Some(name.name().clone()),
                format!("Name \"{}\" is already in use", name.resolve_name()),
            ));
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub(crate) enum EnvVal {
    NoValue,
    Variable(Variable),
    Constant(Constant),
    Function(Function),
    Enum(Enum),
    EnumValue(EnumValue),
    Struct(Struct),
    Trait(Trait),
}

impl EnvVal {
    fn try_to_assign(&mut self, val: Val, name: Token) -> InterpretedResult<()> {
        use EnvVal::*;

        return match self {
            NoValue => Err(RuntimeError::Runtime(
                name,
                String::from("Trying to assign to an immutable value"),
            )),
            Constant(_c) => Err(RuntimeError::Runtime(
                name,
                String::from("Trying to assign to a constant"),
            )),
            Function(_f) => Err(RuntimeError::Runtime(
                name,
                String::from("Trying to assign to an immutable value"),
            )),
            Variable(v) => {
                if let ValType::Uninit = v.v_type {
                    let v_type = try_vtype_from_val(&val);
                    if let Some(v_type) = v_type {
                        v.v_type = v_type;
                        v.val = val;
                        Ok(())
                    } else {
                        Err(RuntimeError::Type(
                            Some(name),
                            format!(
                                "Cannot infer variable type from value of type \"{}\"",
                                val.get_type()
                            ),
                        ))
                    }
                } else if !vtype_conforms_val(&v.v_type, &val) {
                    Err(RuntimeError::Type(
                        Some(name),
                        format!(
                            "Trying to assign to a variable of type \"{}\" value of type \"{}\"",
                            v.v_type,
                            val.get_type()
                        ),
                    ))
                } else if v.mutable || matches!(v.val, Val::Uninit) {
                    v.val = val;
                    Ok(())
                } else {
                    Err(RuntimeError::Runtime(
                        name,
                        String::from("Trying to assign to an immutable variable."),
                    ))
                }
            }
            EnumValue(_) | Enum(_) | Struct(_) | Trait(_) => Err(RuntimeError::Runtime(
                name,
                String::from("Trying to assign to a non-value."),
            )),
        };
    }
}

type NameTarget = (Token, bool);

pub(crate) trait ResolvableName {
    fn resolve_name(&self) -> String {
        if let Some((for_struct, _)) = self.for_target() {
            construct_static_name(&for_struct.lexeme, &self.name().lexeme.clone())
        } else {
            self.name().lexeme.clone()
        }
    }

    fn name(&self) -> &Token;

    fn for_target(&self) -> &Option<NameTarget>;
}

pub(crate) trait ValuableName {
    fn val(&self) -> &Val;
}

#[derive(Clone, Debug)]
pub(crate) struct Variable {
    name: String,
    val: Val,
    v_type: ValType,
    mutable: bool,
}

impl Variable {
    pub(crate) fn new(name: String, val: Val, mutable: bool, v_type: ValType) -> Self {
        Self {
            name,
            val,
            mutable,
            v_type,
        }
    }

    pub(crate) fn val(&self) -> Val {
        if self.v_type == ValType::Any {
            Val::Any(Box::new(self.val.clone()))
        } else {
            self.val.clone()
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Constant {
    name: Token,
    val: Val,
    for_target: Option<NameTarget>,
}

impl Constant {
    pub(crate) fn with_struct(name: Token, val: Val, for_struct: NameTarget) -> Self {
        Self::new(name, val, Some(for_struct))
    }

    pub(crate) fn without_struct(name: Token, val: Val) -> Self {
        Self::new(name, val, None)
    }

    fn new(name: Token, val: Val, for_struct: Option<NameTarget>) -> Self {
        Self {
            name,
            val,
            for_target: for_struct,
        }
    }
}

impl ResolvableName for Constant {
    fn name(&self) -> &Token {
        &self.name
    }

    fn for_target(&self) -> &Option<NameTarget> {
        &self.for_target
    }
}

impl ValuableName for Constant {
    fn val(&self) -> &Val {
        &self.val
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Function {
    name: Token,
    val: Val,
    for_target: Option<NameTarget>,
}

impl Function {
    pub(crate) fn with_struct(name: Token, val: Val, for_struct: NameTarget) -> Self {
        Self::new(name, val, Some(for_struct))
    }

    pub(crate) fn without_struct(name: Token, val: Val) -> Self {
        Self::new(name, val, None)
    }

    fn new(name: Token, val: Val, for_struct: Option<NameTarget>) -> Self {
        Self {
            name,
            val,
            for_target: for_struct,
        }
    }
}

impl ResolvableName for Function {
    fn name(&self) -> &Token {
        &self.name
    }

    fn for_target(&self) -> &Option<NameTarget> {
        &self.for_target
    }
}

impl ValuableName for Function {
    fn val(&self) -> &Val {
        &self.val
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Enum {
    name: Token,
    val: Val,
}

impl Enum {
    pub(crate) fn new(name: Token, val: Val) -> Self {
        Self { name, val }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }
}

#[derive(Clone, Debug)]
pub(crate) struct EnumValue {
    name: Token,
    val: Val,
    for_enum: Token,
}

impl EnumValue {
    pub(crate) fn new(name: Token, val: Val, for_enum: Token) -> Self {
        Self {
            name,
            val,
            for_enum,
        }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }

    fn get_name(&self) -> String {
        construct_static_name(&self.for_enum.lexeme, &self.name.lexeme)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Struct {
    name: String,
    val: Val,
}

impl Struct {
    pub(crate) fn new(name: String, val: Val) -> Self {
        Self { name, val }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Trait {
    name: String,
    methods: Vec<FnSignatureDecl>,
    val: Val,
}

impl Trait {
    pub(crate) fn new(name: String, methods: Vec<FnSignatureDecl>, val: Val) -> Self {
        Self { name, methods, val }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }

    pub(crate) fn methods(&self) -> &[FnSignatureDecl] {
        &self.methods
    }
}

/// It is not wrapped in `EnvValue` enum value,
/// because we store it separately to lookup called functions etc.
#[derive(Clone, Debug)]
pub(crate) struct Impl {
    impl_name: String,
    trait_name: Option<String>,
    methods: Vec<(FnDecl, bool)>,
    fns: Vec<(FnDecl, bool)>,
    consts: Vec<(ConstDecl, bool)>,
}

impl Impl {
    pub(crate) fn new(
        impl_name: String,
        trait_name: Option<String>,
        methods: Vec<(FnDecl, bool)>,
        fns: Vec<(FnDecl, bool)>,
        consts: Vec<(ConstDecl, bool)>,
    ) -> Self {
        Self {
            impl_name,
            trait_name,
            methods,
            fns,
            consts,
        }
    }

    pub(crate) fn trait_name(&self) -> &Option<String> {
        &self.trait_name
    }

    pub(crate) fn methods(&self) -> &[(FnDecl, bool)] {
        &self.methods
    }

    #[allow(dead_code)]
    pub(crate) fn fns(&self) -> &[(FnDecl, bool)] {
        &self.fns
    }

    #[allow(dead_code)]
    pub(crate) fn consts(&self) -> &[(ConstDecl, bool)] {
        &self.consts
    }
}

pub(crate) fn construct_static_name(struct_name: &str, static_field: &str) -> String {
    format!("{}::{}", struct_name, static_field)
}

#[cfg(test)]
mod tests {
    use oxide_parser::{TokenPos, TokenType};

    use super::*;

    #[test]
    fn test_construct_static_name() {
        assert_eq!(construct_static_name("X", "CONST"), "X::CONST");
    }

    #[test]
    fn test_constant_get_name() {
        let constant = Constant::new(
            identifier("test_const"),
            Val::Int(100),
            Some((identifier("test_struct"), true)),
        );

        assert_eq!(constant.resolve_name(), "test_struct::test_const");

        let constant = Constant::new(identifier("test_const"), Val::Int(100), None);

        assert_eq!(constant.resolve_name(), "test_const");
    }

    #[test]
    fn test_function_get_name() {
        let fun = Function::new(
            identifier("test_fn"),
            Val::Int(100),
            Some((identifier("test_struct"), true)),
        );

        assert_eq!(fun.resolve_name(), "test_struct::test_fn");

        let fun = Constant::new(identifier("test_fn"), Val::Int(100), None);

        assert_eq!(fun.resolve_name(), "test_fn");
    }

    #[test]
    fn test_enum_val_get_name() {
        let enum_val = EnumValue::new(
            identifier("test_enum_val"),
            Val::Int(100),
            identifier("test_struct"),
        );

        assert_eq!(enum_val.get_name(), "test_struct::test_enum_val");
    }

    fn identifier(lexeme: &str) -> Token {
        Token::new(
            TokenType::Identifier,
            String::from(lexeme),
            String::from(""),
            TokenPos(0, 0),
        )
    }
}
