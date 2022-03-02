use std::result;

use oxide_parser::stmt::{ConstDecl, FnDecl, FnSignatureDecl};
use oxide_parser::{Token, ValType};

use super::val::Val;
use crate::error::RuntimeError;
use crate::val::{try_vtype_from_val, vtype_conforms_val};

type EnvValOperationResult<T> = result::Result<T, RuntimeError>;

#[derive(Clone, Debug)]
pub(crate) enum EnvVal {
    Variable(Variable),
    Constant(Constant),
    Function(Function),
    Enum(Enum),
    EnumValue(EnumValue),
    Struct(Struct),
    Trait(Trait),
    Type(Type),
}

impl EnvVal {
    pub(crate) fn try_to_assign(&mut self, val: Val, name: Token) -> EnvValOperationResult<()> {
        use EnvVal::*;

        return match self {
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
            EnumValue(_) | Enum(_) | Struct(_) | Trait(_) | Type(_) => Err(RuntimeError::Runtime(
                name,
                String::from("Trying to assign to a non-value."),
            )),
        };
    }
}

#[derive(Clone, Debug)]
pub(crate) struct NameTarget(pub(crate) String, pub(crate) bool);

pub(crate) trait ResolvableName {
    fn resolve_name(&self) -> String {
        if let Some(NameTarget(for_struct, _)) = self.for_target() {
            construct_static_name(for_struct, &self.name().lexeme.clone())
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
    pub(crate) name: String,
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

    pub(crate) fn name(&self) -> &str {
        &self.name.lexeme
    }
}

#[derive(Clone, Debug)]
pub(crate) struct EnumValue {
    name: Token,
    val: Val,
    for_enum: NameTarget,
}

impl EnumValue {
    pub(crate) fn new(name: Token, val: Val, for_enum: NameTarget) -> Self {
        Self {
            name,
            val,
            for_enum,
        }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }

    pub(crate) fn get_name(&self) -> String {
        construct_static_name(&self.for_enum.0, &self.name.lexeme)
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

    pub(crate) fn name(&self) -> &str {
        &self.name
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

    pub fn name(&self) -> &str {
        &self.name
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

    pub(crate) fn impl_name(&self) -> &str {
        &self.impl_name
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Type {
    name: String,
    val: Val,
}

impl Type {
    pub(crate) fn new(name: String, val: Val) -> Self {
        Self { name, val }
    }

    pub(crate) fn val(&self) -> &Val {
        &self.val
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
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
            Some(NameTarget(str::to_string("test_struct"), true)),
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
            Some(NameTarget(str::to_string("test_struct"), true)),
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
            NameTarget(str::to_string("test_struct"), true),
        );

        assert_eq!(enum_val.get_name(), "test_struct::test_enum_val");
    }

    fn identifier(lexeme: &str) -> Token {
        Token::new(TokenType::Identifier, String::from(lexeme), TokenPos(0, 0))
    }
}
