use super::ty::*;
pub use crate::lexer::token::{Token, TokenType};
use crate::parser::expr;
pub use crate::parser::expr::{Expr, Stmt};

use std::collections::HashMap;

struct Scope {
    type_bindings: HashMap<String, Type>,
}

pub struct State {
    in_function_decl: bool,
    returns: Vec<Type>,
}

impl State {
    fn new() -> Self {
        Self {
            in_function_decl: false,
            returns: Vec::new(),
        }
    }
}

pub struct Analyser {
    scope_chain: Vec<Scope>,
    state: State,
}

#[derive(Debug)]
pub enum TypeError {
    MismatchedTypes(Type, Type),
    CannotResolveType,
    InvalidBinaryOperation(Type, Type, Token),
    UndefinedVariable(Token),
    ReturnOutSideFunction,
    MissingReturn,
    ReturnTypeMismatch,
    NotAFunction,
    ArityMismatch,
    FunctionParamsArgsTypeMismatch,
    IfConditionNotBool,
}

pub type AnalyserResult = Result<Type, TypeError>;

impl Analyser {
    pub fn new() -> Self {
        Self {
            scope_chain: Vec::new(),
            state: State::new(),
        }
    }

    pub fn analyse_statements(&mut self, stmts: &[Stmt]) -> AnalyserResult {
        self.enter_scope();
        for stmt in stmts {
            self.analyse_statement(stmt)?;
        }
        self.exit_scope();
        Ok(Type::Nil)
    }
}

impl Analyser {
    fn analyse_expression(&mut self, expression: &Expr) -> AnalyserResult {
        match expression {
            Expr::BinaryExpr(binary_expr) => self.analyse_binary_expression(binary_expr),
            Expr::IntLiteralExpr(_) => Ok(Type::Int),
            Expr::StrLiteralExpr(_) => Ok(Type::Str),
            Expr::BoolLiteralExpr(_) => Ok(Type::Bool),
            Expr::EmptyExpr => Ok(Type::Nil),
            Expr::VariableExpr(variable) => self.analyse_variable(variable),
            Expr::CallExpr(call_expr) => self.analyse_call_expression(call_expr),
            _ => Ok(Type::Nil),
        }
    }

    fn analyse_binary_expression(&mut self, expr: &expr::Binary) -> AnalyserResult {
        let lhs = self.analyse_expression(&*expr.left)?;
        let rhs = self.analyse_expression(&*expr.right)?;

        match (expr.operator.token_type, &lhs, &rhs) {
            (TokenType::Plus, Type::Int, Type::Int) => Ok(Type::Int),
            (TokenType::Plus, Type::Str, Type::Str) => Ok(Type::Str),
            (TokenType::Minus, Type::Int, Type::Int) => Ok(Type::Int),
            _ => Err(TypeError::InvalidBinaryOperation(
                lhs,
                rhs,
                expr.operator.clone(),
            )),
        }
    }

    fn analyse_variable(&mut self, var: &expr::Variable) -> AnalyserResult {
        match self.get_binding_in_scope(&var.name.lexeme) {
            Some(typ) => Ok(typ.clone()),
            None => Err(TypeError::UndefinedVariable(var.name.clone())),
        }
    }

    fn analyse_call_expression(&mut self, call_expr: &expr::Call) -> AnalyserResult {
        // TODO: Method calls
        match &*call_expr.callee {
            Expr::VariableExpr(var_expr) => {
                let name = &var_expr.name.lexeme;

                let mut arg_types: Vec<Type> = Vec::new();
                for expr in &call_expr.args {
                    let ty = self.analyse_expression(expr)?;
                    arg_types.push(ty);
                }

                let ty = match self.get_binding_in_scope(name) {
                    Some(ty) => ty,
                    None => return Err(TypeError::NotAFunction),
                };
                let function = match ty {
                    Type::Function(function) => function,
                    _ => return Err(TypeError::NotAFunction),
                };
                let return_type = &function.return_type;
                let param_types = &function.param_types;
                let arity = function.param_types.len();

                if arg_types.len() != arity {
                    return Err(TypeError::ArityMismatch);
                }
                let types_match = arg_types.iter().zip(param_types).all(|(t1, t2)| t1 == t2);

                if types_match {
                    Ok(return_type.clone())
                } else {
                    Err(TypeError::FunctionParamsArgsTypeMismatch)
                }
            }
            _ => Err(TypeError::NotAFunction),
        }
    }
}

// MARK: Analyse Statements
impl Analyser {
    fn analyse_statement(&mut self, stmt: &Stmt) -> AnalyserResult {
        match stmt {
            Stmt::Let(var_decl) => self.analyse_var_decl(var_decl),
            Stmt::BlockStmt(block) => self.analyse_block_statement(block),
            Stmt::Expr(expr) => self.analyse_expression(expr),
            Stmt::Fn(fn_decl) => self.analyse_fn_decl(fn_decl),
            Stmt::Return(return_stmt) => self.analyse_return_statement(return_stmt),
            Stmt::IfStmt(if_stmt) => self.analyse_if_statement(if_stmt),
            _ => Ok(Type::Nil),
        }
    }

    fn analyse_var_decl(&mut self, var_decl: &expr::VarDecl) -> AnalyserResult {
        let typ = match (&*var_decl.init, &var_decl.v_type) {
            (Some(expr), Some(typ)) => {
                let expr_type = self.analyse_expression(expr)?;
                let typ = Type::from(&typ);
                if typ != expr_type {
                    return Err(TypeError::MismatchedTypes(typ, expr_type));
                }
                expr_type
            }
            (Some(expr), None) => self.analyse_expression(expr)?,
            (None, Some(typ)) => Type::from(typ),
            _ => return Err(TypeError::CannotResolveType),
        };
        let name = var_decl.name.lexeme.clone();
        self.create_binding_in_scope(name, typ);
        Ok(Type::Nil)
    }

    fn analyse_fn_decl(&mut self, function: &expr::FnDecl) -> AnalyserResult {
        let lambda = &function.lambda;
        self.state.in_function_decl = true;

        let return_type = Type::from(&lambda.ret_type);

        self.enter_scope();

        // TODO: Consider mutability of params
        let param_types: Vec<Type> = (&lambda.params)
            .iter()
            .map(|(token, typ, _is_mutable)| {
                // let (token, typ, _is_mutable) = param;
                let typ = Type::from(&typ);
                self.create_binding_in_scope(token.lexeme.clone(), typ.clone());
                typ
            })
            .collect();

        for stmt in &lambda.body {
            self.analyse_statement(stmt)?;
        }

        if return_type != Type::Nil {
            if self.state.returns.is_empty() {
                return Err(TypeError::MissingReturn);
            }
            for return_expr in &self.state.returns {
                if *return_expr != return_type {
                    return Err(TypeError::ReturnTypeMismatch);
                }
            }
        }

        self.exit_scope();
        self.state.in_function_decl = false;
        let fn_ty = FunctionType::new(return_type, param_types);
        dbg!(&fn_ty);
        let fn_name = function.name.lexeme.clone();
        self.create_binding_in_scope(fn_name, Type::Function(Box::new(fn_ty)));
        Ok(Type::Nil)
    }

    fn analyse_if_statement(&mut self, if_stmt: &expr::If) -> AnalyserResult {
        if self.analyse_expression(&*if_stmt.condition)? != Type::Bool {
            return Err(TypeError::IfConditionNotBool);
        }
        self.analyse_statement(&*if_stmt.then_stmt)?;
        if let Some(else_stmt) = &if_stmt.else_stmt {
            self.analyse_statement(&*else_stmt)?;
        }
        Ok(Type::Nil)
    }

    fn analyse_block_statement(&mut self, block_stmt: &expr::Block) -> AnalyserResult {
        self.enter_scope();
        for stmt in &block_stmt.stmts {
            self.analyse_statement(stmt)?;
        }
        self.exit_scope();

        Ok(Type::Nil)
    }

    fn analyse_return_statement(&mut self, return_stmt: &expr::Return) -> AnalyserResult {
        if !self.state.in_function_decl {
            return Err(TypeError::ReturnOutSideFunction);
        }

        let expr_type = self.analyse_expression(&*return_stmt.expr)?;
        self.state.returns.push(expr_type);
        Ok(Type::Nil)
    }
}

impl Analyser {
    fn enter_scope(&mut self) {
        self.scope_chain.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scope_chain.pop();
    }

    fn create_binding_in_scope(&mut self, name: String, typ: Type) {
        self.scope_chain
            .last_mut()
            .unwrap()
            .create_binding_for(name, typ)
    }

    fn get_binding_in_scope(&self, name: &str) -> Option<&Type> {
        self.scope_chain.last().unwrap().get_binding_for(name)
    }
}

// MARK: Scope

impl Scope {
    fn get_binding_for(&self, name: &str) -> Option<&Type> {
        self.type_bindings.get(name)
    }

    fn create_binding_for(&mut self, name: String, typ: Type) {
        self.type_bindings.insert(name, typ);
    }

    fn new() -> Self {
        Self {
            type_bindings: HashMap::new(),
        }
    }
}
