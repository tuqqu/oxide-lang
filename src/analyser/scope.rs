use super::*;
use crate::lexer::token::Token;
use std::collections::HashMap;

pub struct TypeBinding {
    ty: Type,
    at: Token,
    is_mutable: bool,
    is_initialised: bool,
}

impl TypeBinding {
    pub fn new(ty: Type, at: Token, is_mutable: bool, is_initialised: bool) -> Self {
        Self {
            ty,
            at,
            is_mutable,
            is_initialised,
        }
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

    pub fn is_initialised(&self) -> bool {
        self.is_initialised
    }

    pub fn make_initialised(&mut self) {
        self.is_initialised = true
    }
}

pub struct Scope {
    type_bindings: HashMap<String, TypeBinding>,
}

impl Scope {
    pub fn get_binding_for(&self, name: &str) -> Option<&TypeBinding> {
        self.type_bindings.get(name)
    }

    pub fn get_mut_binding_for(&mut self, name: &str) -> Option<&mut TypeBinding> {
        self.type_bindings.get_mut(name)
    }

    pub fn create_binding_for(
        &mut self,
        name: String,
        ty: Type,
        at: Token,
        is_mutable: bool,
        is_initialised: bool,
    ) {
        let binding = TypeBinding::new(ty, at, is_mutable, is_initialised);
        self.type_bindings.insert(name, binding);
    }

    pub fn new() -> Self {
        Self {
            type_bindings: HashMap::new(),
        }
    }

    pub fn all_bindings_iter(&self) -> std::collections::hash_map::Iter<String, TypeBinding> {
        self.type_bindings.iter()
    }
}
