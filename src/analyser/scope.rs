use super::*;
use crate::lexer::token::Token;
use std::collections::HashMap;

pub struct TypeBinding {
    ty: Type,
    at: Token,
    is_mutable: bool,
}

impl TypeBinding {
    pub fn new(ty: Type, at: Token, is_mutable: bool) -> Self {
        Self { ty, at, is_mutable }
    }

    pub fn get_ty(&self) -> &Type {
        &self.ty
    }

    pub fn get_at(&self) -> &Token {
        &self.at
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

pub struct Scope {
    type_bindings: HashMap<String, TypeBinding>,
}

impl Scope {
    pub fn get_binding_for(&self, name: &str) -> Option<&TypeBinding> {
        self.type_bindings.get(name)
    }

    pub fn create_binding_for(&mut self, name: String, ty: Type, at: Token, is_mutable: bool) {
        let binding = TypeBinding::new(ty, at, is_mutable);
        self.type_bindings.insert(name, binding);
    }

    pub fn new() -> Self {
        Self {
            type_bindings: HashMap::new(),
        }
    }
}
