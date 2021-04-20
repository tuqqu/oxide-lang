use super::*;
use crate::lexer::token::{Token, TokenType};
use crate::parser::{
    expr,
    expr::{Expr, Stmt},
};

pub struct State {
    in_function_decl: bool,
    returns: Vec<(/*ty:*/ Type, /*location:*/ Token)>,
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
            Expr::AssignmentExpr(assignment) => self.analyse_assignment(assignment),
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
            Some(binding) => {
                if !binding.is_initialised() {
                    return Err(TypeError::UninitialisedVariableAccess(var.name.clone()));
                }
                Ok(binding.get_ty().clone())
            }
            None => Err(TypeError::UndefinedVariable(var.name.clone())),
        }
    }

    fn analyse_call_expression(&mut self, call_expr: &expr::Call) -> AnalyserResult {
        // TODO: Method calls
        let var_expr = match &*call_expr.callee {
            Expr::VariableExpr(var_expr) => var_expr,
            _ => return Err(TypeError::NotAFunction),
        };
        let name = &var_expr.name.lexeme; 

        // Analyse args
        let arg_tys:Vec<Type> = 
        call_expr.args.iter().map(|expr| {
            self.analyse_expression(expr).unwrap_or(Type::Nil)
        }).collect();

        let function = match self.get_binding_in_scope(name) {
            Some(binding) => {
                match binding.get_ty() {
                    Type::Function(function) => function,
                    _ => return Err(TypeError::NotAFunction)
                }
            },
            None => return Err(TypeError::NotAFunction)
        };

        let param_types = &function.param_types;
        let return_type = &function.return_type;

        if arg_tys.len() != param_types.len() {
            return Err(TypeError::ArityMismatch)
        }
        for (arg_ty, param_ty) in arg_tys.iter().zip(param_types) {
            if arg_ty != param_ty {
                return Err(TypeError::FunctionParamsArgsTypeMismatch);
            }
        }

        Ok(return_type.clone())
    }

    fn analyse_assignment(&mut self, assignment: &expr::Assignment) -> AnalyserResult {
        let name = &assignment.name.lexeme;
        let binding = match self.get_binding_in_scope(name) {
            Some(binding) => binding,
            None => return Err(TypeError::UndefinedVariable(assignment.name.clone())),
        };

        if !binding.is_mutable() {
            return Err(TypeError::cannot_mutate_variable(
                assignment.name.clone(),
                binding.get_at().clone(),
            ));
        }

        let expr = &*assignment.expr;
        let expr_ty = self.analyse_expression(expr)?;

        let binding = self.get_mut_binding_in_scope(name).unwrap();
        if expr_ty != *binding.get_ty() {
            let expected = binding.get_ty().clone();
            let at = binding.get_at().clone();
            return Err(TypeError::type_mismatch(
                expected,
                at,
                expr_ty,
                assignment.name.clone(),
            ));
        }

        binding.make_initialised();
        Ok(expr_ty)
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
                    return Err(TypeError::AmbiguousTypes);
                }
                expr_type
            }
            (Some(expr), None) => self.analyse_expression(expr)?,
            (None, Some(typ)) => Type::from(typ),
            _ => return Err(TypeError::CannotResolveType),
        };

        let name = var_decl.name.lexeme.clone();
        self.create_binding_in_scope(name, typ, var_decl.name.clone(), var_decl.mutable, var_decl.init.is_some());
        Ok(Type::Nil)
    }

    fn analyse_fn_decl(&mut self, function: &expr::FnDecl) -> AnalyserResult {
        let lambda = &function.lambda;
        let return_type = Type::from(&lambda.ret_type);

        self.enter_function_decl();

        // Bind params to scope
        let param_types: Vec<Type> = (&lambda.params)
            .iter()
            .map(|(token, typ, is_mutable)| {
                // TODO: Consider mutability of params
                let typ = Type::from(&typ);
                self.create_binding_in_scope(
                    token.lexeme.clone(),
                    typ.clone(),
                    token.clone(),
                    *is_mutable,
                    true, // params are set at the callsite
                );
                typ
            })
            .collect();

        // Analyse FunctionBody
        for stmt in &lambda.body {
            self.analyse_statement(stmt)?;
        }

        // Check returns
        // TODO: Return Err(TypeError::MissingReturn)
        for (return_ty, token) in &self.state.returns {
            if *return_ty != return_type {
                return Err(TypeError::return_type_mismatch(
                    return_type,
                    function.name.clone(),
                    return_ty.clone(),
                    token.clone(),
                ));
            }
        }

        self.exit_function_decl();
        let fn_ty = FunctionType::new(return_type, param_types);
        let fn_name = function.name.lexeme.clone();
        let ty = Type::Function(Box::new(fn_ty));

        self.create_binding_in_scope(fn_name, ty, function.name.clone(), false, true);
        Ok(Type::Nil)
    }

    fn analyse_if_statement(&mut self, if_stmt: &expr::If) -> AnalyserResult {
        if self.analyse_expression(&*if_stmt.condition)? != Type::Bool {
            return Err(TypeError::ConditionNotBool);
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
        self.state
            .returns
            .push((expr_type, return_stmt.keyword.clone()));
        Ok(Type::Nil)
    }
}

// MARK: Utils

impl Analyser {
    fn enter_scope(&mut self) {
        self.scope_chain.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scope_chain.pop();
    }

    fn create_binding_in_scope(&mut self, name: String, ty: Type, at: Token, is_mutable: bool, is_initialised : bool) {
        self.scope_chain
            .last_mut()
            .unwrap()
            .create_binding_for(name, ty, at, is_mutable, is_initialised)
    }

    fn get_binding_in_scope(&self, name: &str) -> Option<&TypeBinding> {
        self.scope_chain.last().unwrap().get_binding_for(name)
    }

    fn get_mut_binding_in_scope(&mut self, name: &str) -> Option<&mut TypeBinding> {
        self.scope_chain.last_mut().unwrap().get_mut_binding_for(name)
    }

    fn enter_function_decl(&mut self) {
        self.state.in_function_decl = true;
        self.enter_scope();
    }

    fn exit_function_decl(&mut self) {
        self.state.in_function_decl = false;
        self.state.returns.clear();
        self.exit_scope();
    }
}
