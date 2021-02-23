use std::result;

use crate::parser::expr::Expr::{GetPropExpr, IntLiteralExpr, SetPropExpr};
use crate::parser::expr::{
    CallStruct, GetProp, IntLiteral, Lambda, Self_, SetProp, StructDecl, ValType,
};
use crate::{error, error_token, Token, TokenType};

use self::expr::Expr::{
    AssignmentExpr, BinaryExpr, BoolLiteralExpr, EmptyExpr, FloatLiteralExpr, GroupingExpr,
    LogicalBinaryExpr, NilLiteralExpr, StrLiteralExpr, UnaryExpr, VariableExpr,
};
use self::expr::Stmt::{BlockStmt, IfStmt, LoopStmt};
use self::expr::{
    Assignment, Binary, Block, BoolLiteral, Call, ConstDecl, Expr, FloatLiteral, FnDecl, Grouping,
    If, Loop, NilLiteral, Return, Stmt, StrLiteral, Unary, VarDecl, Variable,
};

pub mod expr;

pub type Result<T> = result::Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError;

pub struct Parser {
    pub tokens: Vec<Token>,
    current: usize,
    loop_depth: usize,
    fn_depth: usize,
    err: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            loop_depth: 0,
            fn_depth: 0,
            err: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts: Vec<Stmt> = Vec::<Stmt>::new();

        while !self.at_end() {
            let decl = self.decl_stmt();
            match decl {
                Some(decl) => stmts.push(decl),
                None => {}
            }
        }

        if self.err {
            Err(ParserError)
        } else {
            Ok(stmts)
        }
    }

    fn decl_stmt(&mut self) -> Option<Stmt> {
        if self.match_token(TokenType::Let) {
            return match self.var_decl() {
                Ok(var_decl) => Some(var_decl),
                Err(_) => {
                    self.try_to_recover();
                    None
                }
            };
        } else if self.match_token(TokenType::Const) {
            return match self.const_decl() {
                Ok(const_decl) => Some(const_decl),
                Err(_) => {
                    self.try_to_recover();
                    None
                }
            };
        } else if self.check(TokenType::Fn) && self.check_next(TokenType::Identifier) {
            self.consume(TokenType::Fn, "Keyword \"fn\" expected.".to_string()).unwrap();

            return match self.fn_decl() {
                Ok(fn_decl) => Some(fn_decl),
                Err(_) => {
                    self.try_to_recover();
                    None
                }
            };
        } else if self.match_token(TokenType::Struct) {
            return match self.struct_decl() {
                Ok(fn_decl) => Some(fn_decl),
                Err(_) => {
                    self.try_to_recover();
                    None
                }
            };
        }

        return match self.any_stmt() {
            Ok(stmt) => Some(stmt),
            Err(_) => {
                self.try_to_recover();
                None
            }
        };
    }

    /// Parses variable mutable and immutable declaration syntax,
    /// as well as variable type and initializer.
    fn var_decl(&mut self) -> Result<Stmt> {
        let mutable: bool = self.match_token(TokenType::Mut);
        let name: Token = self.consume(TokenType::Identifier, "Variable name expected.".to_string())?;

        let v_type = if self.match_token(TokenType::Colon) {
            Some(self.type_decl()?)
        } else {
            None
        };

        let init: Option<Expr> = if self.match_token(TokenType::Equal) {
            Some(self.any_expr()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after variable declaration".to_string(),
        )?;
        let var_decl_stmt = Stmt::Let(VarDecl::new(name, Box::new(init), mutable, v_type));

        Ok(var_decl_stmt)
    }

    /// Struct properties have their own syntax which is different from usual vars,
    /// so they must be handled separately
    fn prop_decl(&mut self) -> Result<VarDecl> {
        let name: Token = self.consume(TokenType::Identifier, "Property name expected.".to_string())?;

        self.consume(
            TokenType::Colon,
            "Colon \":\" expected after after prop declaration".to_string(),
        )?;

        let v_type = self.type_decl()?;

        self.consume(
            TokenType::Comma,
            "Comma \",\" expected after property declaration".to_string(),
        )?;

        let var_decl = VarDecl::new(name, Box::new(None), true, Some(v_type));

        Ok(var_decl)
    }

    /// Type declaration for anything: variable, argument, function return type etc
    fn type_decl(&mut self) -> Result<ValType> {
        let v_type_token: Token = self.consume_type()?;
        let v_type = ValType::try_from_token(&v_type_token);
        match v_type {
            Some(v_type) => Ok(v_type),
            None => {
                self.err = true;
                error_token(&v_type_token, "Unrecognised type.".to_string());
                Err(ParserError)
            }
        }
    }

    fn const_decl(&mut self) -> Result<Stmt> {
        let name: Token = self.consume(TokenType::Identifier, "Constant name expected.".to_string())?;

        let init = if self.match_token(TokenType::Equal) {
            if let Some(expr) = self.scalar_expr() {
                expr
            } else {
                self.err = true;
                error_token(
                    &self.tokens[self.current],
                    "Constant must be initialized with a scalar value only.".to_string(),
                );
                return Err(ParserError);
            }
        } else {
            self.err = true;
            error_token(
                &self.tokens[self.current],
                "Constant must be initialized.".to_string(),
            );
            return Err(ParserError);
        };

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after const declaration".to_string(),
        )?;

        let const_decl_stmt = Stmt::Const(ConstDecl::new(name, Box::new(init)));

        Ok(const_decl_stmt)
    }

    /// Parses the standalone function declaration statement.
    fn fn_decl(&mut self) -> Result<Stmt> {
        let fn_decl_stmt = Stmt::Fn(self.fn_decl_inner()?);

        Ok(fn_decl_stmt)
    }

    /// Used by both function declaration and struct method declaration.
    /// Returns the inner FnDecl struct, which represents the declaration itself,
    /// without it being a statement
    fn fn_decl_inner(&mut self) -> Result<FnDecl> {
        let name: Token =
            self.consume(TokenType::Identifier, "Function name expected.".to_string())?; //handle err

        let lambda = self.lambda_expr()?;

        let lambda = match lambda {
            Expr::FnExpr(l) => l,
            _ => {
                self.err = true;
                error_token(
                    &self.tokens[self.current],
                    "Constant must have a value.".to_string(),
                );
                return Err(ParserError);
            }
        };

        let fn_decl = FnDecl::new(name, lambda);

        Ok(fn_decl)
    }

    /// Parses the struct declaration statement as well as all its members:
    /// struct properties and its methods.
    /// future scope: move methods to `impl` blocks
    fn struct_decl(&mut self) -> Result<Stmt> {
        let name: Token = self.consume(TokenType::Identifier, "Struct name expected.".to_string())?;

        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected after struct name".to_string(),
        )?;

        let mut fns = vec![];
        let mut props = vec![];

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            if self.check(TokenType::Identifier) {
                match self.prop_decl() {
                    Ok(var_decl) => {
                        props.push(var_decl);
                    }
                    Err(_) => {
                        self.try_to_recover();
                    }
                };
            } else if self.match_token(TokenType::Fn) {
                match self.fn_decl_inner() {
                    Ok(fn_decl) => {
                        fns.push(fn_decl);
                    }
                    Err(_) => {
                        self.try_to_recover();
                    }
                };
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after struct body".to_string(),
        )?;

        let struct_decl_stmt = Stmt::Struct(StructDecl::new(name, props, fns));

        Ok(struct_decl_stmt)
    }

    fn lambda_expr(&mut self) -> Result<Expr> {
        self.fn_depth += 1;

        self.consume(
            TokenType::LeftParen,
            "Parenthesis \"(\" expected after function name".to_string(),
        )?;

        let mut params: Vec<(Token, ValType, bool)> = vec![];

        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() > FnDecl::MAX_ARGS {
                    self.err = true;
                    error_token(self.peek(), "Block statement expected.".to_string());
                }

                let mutable = self.check(TokenType::Mut);
                if mutable {
                    self.advance();
                }

                let id = self.consume(
                    TokenType::Identifier,
                    "Expected argument of a function".to_string(),
                )?;

                let v_type = if self.match_token(TokenType::Colon) {
                    self.type_decl()?
                } else {
                    self.err = true;
                    error_token(
                        &id,
                        format!(
                            "Function argument \"{}\" must be explicitly typed.",
                            id.lexeme
                        ),
                    );
                    return Err(ParserError);
                };

                params.push((id, v_type, mutable));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Parenthesis \")\" expected after function name".to_string(),
        )?;

        let ret_type = if self.match_token(TokenType::Arrow) {
            self.type_decl()?
        } else {
            ValType::Nil
        };

        let body = self.block_stmt()?;

        let body = if let BlockStmt(block) = body {
            block
        } else {
            self.err = true;
            error_token(self.peek(), "Function body error.".to_string());
            return Err(ParserError);
        };

        let lambda_expr = Expr::FnExpr(Lambda::new(params, ret_type, body.stmts));

        self.fn_depth -= 1;
        Ok(lambda_expr)
    }

    fn any_stmt(&mut self) -> Result<Stmt> {
        if self.match_token(TokenType::If) {
            return self.if_stmt();
        }

        if self.match_token(TokenType::While) {
            return self.while_stmt();
        }

        if self.match_token(TokenType::For) {
            return self.for_stmt();
        }

        if self.match_token(TokenType::Loop) {
            return self.loop_stmt();
        }

        if self.match_token(TokenType::Break) {
            return self.break_stmt();
        }

        if self.match_token(TokenType::Continue) {
            return self.continue_stmt();
        }

        if self.match_token(TokenType::Return) {
            return self.return_stmt();
        }

        if self.match_token(TokenType::LeftCurlyBrace) {
            let block_stmt = BlockStmt(Block::new(self.block()?));

            return Ok(block_stmt);
        }

        self.expr_stmt()
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        let condition = Box::new(self.any_expr()?);
        let then_stmt = Box::new(self.block_stmt()?);
        let else_stmt = if self.match_token(TokenType::Else) {
            let stmt = if self.match_token(TokenType::If) {
                self.if_stmt()?
            } else {
                self.block_stmt()?
            };
            Some(Box::new(stmt))
        } else {
            None
        };

        let if_stmt = IfStmt(If::new(condition, then_stmt, else_stmt));

        Ok(if_stmt)
    }

    fn while_stmt(&mut self) -> Result<Stmt> {
        self.loop_depth += 1;
        let condition = match self.any_expr() {
            Ok(expr) => expr,
            Err(e) => {
                self.loop_depth -= 1;
                return Err(e);
            }
        };

        let body = match self.block_stmt() {
            Ok(stmt) => stmt,
            Err(e) => {
                self.loop_depth -= 1;
                return Err(e);
            }
        };

        let while_stmt = LoopStmt(Loop::new(
            Box::new(Expr::EmptyExpr),
            Box::new(condition),
            Box::new(body),
        ));

        Ok(while_stmt)
    }

    fn loop_stmt(&mut self) -> Result<Stmt> {
        self.loop_depth += 1;

        let body = match self.block_stmt() {
            Ok(stmt) => stmt,
            Err(e) => {
                self.loop_depth -= 1;
                return Err(e);
            }
        };

        let loop_stmt = LoopStmt(Loop::new(
            Box::new(Expr::EmptyExpr),
            Box::new(Expr::BoolLiteralExpr(BoolLiteral(true))),
            Box::new(body),
        ));

        Ok(loop_stmt)
    }

    fn for_stmt(&mut self) -> Result<Stmt> {
        self.loop_depth += 1;

        let init = if self.match_token(TokenType::Semicolon) {
            None
        } else if self.match_token(TokenType::Let) {
            let var_decl = match self.var_decl() {
                Ok(stmt) => stmt,
                Err(e) => {
                    self.loop_depth -= 1;
                    return Err(e);
                }
            };

            Some(var_decl)
        } else {
            let expr_stmt = match self.expr_stmt() {
                Ok(stmt) => stmt,
                Err(e) => {
                    self.loop_depth -= 1;
                    return Err(e);
                }
            };

            Some(expr_stmt)
        };

        let condition = match self.any_expr() {
            Ok(expr) => expr,
            Err(e) => {
                self.loop_depth -= 1;
                return Err(e);
            }
        };

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after loop condition.".to_string(),
        )?;

        let inc = Box::new(if self.check(TokenType::LeftCurlyBrace) {
            Expr::EmptyExpr
        } else {
            let expr = match self.any_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    self.loop_depth -= 1;
                    return Err(e);
                }
            };

            expr
        });

        let block = match self.block_stmt() {
            Ok(stmt) => stmt,
            Err(e) => {
                self.loop_depth -= 1;
                return Err(e);
            }
        };

        let body = Box::new(block);

        let condition = Box::new(if condition.is_empty() {
            Expr::BoolLiteralExpr(BoolLiteral(true))
        } else {
            condition
        });

        let mut for_stmt = LoopStmt(Loop::new(inc, condition, body));

        if init.is_some() {
            for_stmt = Stmt::BlockStmt(Block::new(vec![init.unwrap(), for_stmt]));
        }

        Ok(for_stmt)
    }

    fn break_stmt(&mut self) -> Result<Stmt> {
        if self.loop_depth == 0 {
            self.err = true;
            error_token(
                &self.previous(),
                "Must be inside a loop to use \"break\".".to_string(),
            );
        }

        self.consume(TokenType::Semicolon, "Semicolon \";\" expected after break.".to_string())?;

        Ok(Stmt::Break)
    }

    fn continue_stmt(&mut self) -> Result<Stmt> {
        if self.loop_depth == 0 {
            self.err = true;
            error_token(
                &self.previous(),
                "Must be inside a loop to use \"continue\".".to_string(),
            );
        }

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after continue.".to_string(),
        )?;

        Ok(Stmt::Continue)
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        if 0 == self.fn_depth {
            self.err = true;
            error_token(
                &self.previous(),
                "Must be inside a function to use \"return\".".to_string(),
            );
        }

        let token = self.previous().clone();
        let expr = if !self.check(TokenType::Semicolon) {
            self.any_expr()?
        } else {
            Expr::EmptyExpr
        };

        self.consume(TokenType::Semicolon, "Semicolon \";\" expected after return.".to_string())?;

        Ok(Stmt::Return(Return::new(token, Box::new(expr))))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.any_expr()?;
        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after expression".to_string(),
        )?;
        let expr_stmt = Stmt::Expr(expr);

        Ok(expr_stmt)
    }

    fn block_stmt(&mut self) -> Result<Stmt> {
        if self.match_token(TokenType::LeftCurlyBrace) {
            let block_stmt = BlockStmt(Block::new(self.block()?));

            return Ok(block_stmt);
        }

        self.err = true;
        error_token(
            &self.tokens[self.current],
            "Block statement expected.".to_string(),
        );
        Err(ParserError)
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::<Stmt>::new();

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            match self.decl_stmt() {
                Some(decl) => stmts.push(decl),
                None => {}
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after block.".to_string(),
        )?;

        Ok(stmts)
    }

    fn any_expr(&mut self) -> Result<Expr> {
        let expr = self.assign_expr()?;

        Ok(expr)
    }

    fn assign_expr(&mut self) -> Result<Expr> {
        let expr = self.logic_or()?;

        if self.match_tokens(vec![
            TokenType::Equal,
            TokenType::SlashEqual,
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::AsteriskEqual,
            TokenType::ModulusEqual,
        ]) {
            let operator: Token = self.previous().clone();
            let expr_val = self.assign_expr()?;

            return match &expr {
                VariableExpr(variable) => Ok(AssignmentExpr(Assignment::new(
                    variable.name.clone(),
                    operator,
                    Box::new(expr_val),
                ))),
                GetPropExpr(get_prop) => {
                    match &*get_prop.name {
                        VariableExpr(variable) => Ok(SetPropExpr(SetProp::new(
                            variable.name.clone(),
                            get_prop.prop_name.clone(),
                            operator,
                            Box::new(expr_val),
                        ))),
                        // recursive call to follow all the dot chain
                        GetPropExpr(_) => self.assign_expr(),
                        _ => panic!("Unknown error in assignment"),
                    }
                }
                _ => {
                    self.err = true;
                    error_token(&operator, "Invalid assignment target.".to_string());
                    Err(ParserError)
                }
            };
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr> {
        let mut expr = self.logic_and()?;

        while self.match_token(TokenType::LogicOr) {
            let operator = self.previous().clone();
            let right = self.logic_and()?;
            expr = LogicalBinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr> {
        let mut expr = self.equality_expr()?;

        while self.match_token(TokenType::LogicAnd) {
            let operator = self.previous().clone();
            let right = self.equality_expr()?;
            expr = LogicalBinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Expr> {
        let mut expr = self.comparison_expr()?;

        while self.match_tokens(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.comparison_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Expr> {
        let mut expr = self.sum_expr()?;

        while self.match_tokens(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.sum_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn sum_expr(&mut self) -> Result<Expr> {
        let mut expr = self.mult_expr()?;

        while self.match_tokens(vec![TokenType::Minus, TokenType::Plus]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.mult_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn mult_expr(&mut self) -> Result<Expr> {
        let mut expr = self.unary_expr()?;

        while self.match_tokens(vec![
            TokenType::Asterisk,
            TokenType::Slash,
            TokenType::Modulus,
        ]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Expr> {
        if self.match_tokens(vec![TokenType::Bang, TokenType::Minus]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary_expr()?;
            let unary = UnaryExpr(Unary::new(Box::new(right), operator));

            return Ok(unary);
        }

        self.call_expr()
    }

    fn call_expr(&mut self) -> Result<Expr> {
        let mut expr = self.primary_expr()?;

        match expr {
            VariableExpr(_) => {
                if self.match_token(TokenType::LeftCurlyBrace) {
                    expr = self.finish_struct_call_expr(expr)?
                }
            }
            _ => {}
        }

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_call_expr(expr)?;
            } else if self.match_token(TokenType::Dot) {
                let name = self.consume(
                    TokenType::Identifier,
                    "Property name ecpected after \".\".".to_string(),
                )?;
                expr = Expr::GetPropExpr(GetProp::new(Box::new(expr), name));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call_expr(&mut self, callee: Expr) -> Result<Expr> {
        let mut args = vec![];

        if !self.check(TokenType::RightParen) {
            loop {
                if args.len() >= FnDecl::MAX_ARGS {
                    self.err = true;
                    error(
                        self.peek().line,
                        format!("Can\"t have more than {} arguments.", FnDecl::MAX_ARGS),
                    );
                }

                args.push(self.any_expr()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            "Parenthesis \")\" expected after arguments.".to_string(),
        )?;

        let call = Expr::CallExpr(Call::new(Box::new(callee), args));

        Ok(call)
    }

    fn finish_struct_call_expr(&mut self, callee: Expr) -> Result<Expr> {
        let mut args = vec![];

        if !self.check(TokenType::RightCurlyBrace) {
            loop {
                if args.len() >= FnDecl::MAX_ARGS {
                    self.err = true;
                    error(
                        self.peek().line,
                        format!("Can\"t have more than {} arguments.", FnDecl::MAX_ARGS),
                    );
                }

                let prop = self.consume(TokenType::Identifier, "Property name expected.".to_string())?;
                self.consume(TokenType::Colon, "Colon \":\" expected after after prop.".to_string())?;

                args.push((prop, self.any_expr()?));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        let _cl_paren = self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after arguments.".to_string(),
        );

        let call = Expr::CallStructExpr(CallStruct::new(Box::new(callee), args));

        Ok(call)
    }

    fn primary_expr(&mut self) -> Result<Expr> {
        if let Some(expr) = self.scalar_expr() {
            return Ok(expr);
        }

        if self.match_token(TokenType::Self_) {
            return Ok(Expr::SelfExpr(Self_::new(self.previous().clone())));
        }

        if self.match_token(TokenType::Identifier) {
            return Ok(VariableExpr(Variable::new(self.previous().clone())));
        }

        if self.match_token(TokenType::Fn) {
            return self.lambda_expr();
        }

        if self.match_token(TokenType::LeftParen) {
            let expr = self.any_expr()?;
            self.consume(
                TokenType::RightParen,
                "Parenthesis \")\" expected after expression.".to_string(),
            )?;
            let grouping_expr = GroupingExpr(Grouping::new(Box::new(expr)));

            return Ok(grouping_expr);
        }

        if self.check(TokenType::Semicolon) {
            return Ok(EmptyExpr);
        }

        self.err = true;
        error_token(self.peek(), "Expression expected.".to_string());
        Err(ParserError)
    }

    fn scalar_expr(&mut self) -> Option<Expr> {
        if self.match_token(TokenType::False) {
            return Some(BoolLiteralExpr(BoolLiteral(false)));
        }

        if self.match_token(TokenType::True) {
            return Some(BoolLiteralExpr(BoolLiteral(true)));
        }

        if self.match_token(TokenType::Nil) {
            return Some(NilLiteralExpr(NilLiteral));
        }

        if self.match_token(TokenType::NumberFloat) {
            let n = self.previous().literal.parse::<f64>().unwrap();
            return Some(FloatLiteralExpr(FloatLiteral(n)));
        }

        if self.match_token(TokenType::NumberInt) {
            let n = self.previous().literal.parse::<isize>().unwrap();
            return Some(IntLiteralExpr(IntLiteral(n)));
        }

        if self.match_token(TokenType::String) {
            return Some(StrLiteralExpr(StrLiteral(self.previous().literal.clone())));
        }

        None
    }

    fn match_tokens(&mut self, t_types: Vec<TokenType>) -> bool {
        for t_type in t_types {
            if self.match_token(t_type) {
                return true;
            }
        }

        false
    }

    fn match_token(&mut self, t_type: TokenType) -> bool {
        if self.check(t_type) {
            self.advance();
            return true;
        }

        false
    }

    fn check(&self, t_type: TokenType) -> bool {
        if self.at_end() {
            return false;
        }

        self.peek().token_type == t_type
    }

    fn check_next(&self, t_type: TokenType) -> bool {
        if self.at_end() {
            return false;
        }

        if let Some(token) = self.tokens.get(self.current + 1) {
            match token.token_type {
                TokenType::Eof => false,
                next_t_type => t_type == next_t_type,
            }
        } else {
            false
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        self.peek_by(0)
    }

    fn peek_by(&self, by: usize) -> &Token {
        self.tokens.get(self.current + by).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn consume_type(&mut self) -> Result<Token> {
        use TokenType::*;

        if !self.check(Num)
            && !self.check(Float)
            && !self.check(Int)
            && !self.check(Nil)
            && !self.check(Str)
            && !self.check(Bool)
            && !self.check(Vec)
            && !self.check(Map)
            && !self.check(Any)
            && !self.check(Func)
            && !self.check(Identifier)
        {
            self.err = true;
            error_token(self.peek(), "Type expected.".to_string());

            return Err(ParserError);
        }

        Ok(self.advance().clone())
    }

    fn consume(&mut self, t_type: TokenType, msg: String) -> Result<Token> {
        if !self.check(t_type) {
            self.err = true;
            error_token(self.peek(), msg);

            return Err(ParserError);
        }

        Ok(self.advance().clone())
    }

    fn try_to_recover(&mut self) {
        use TokenType::*;

        self.advance();

        while !self.at_end() {
            if self.previous().token_type == Semicolon {
                return;
            }

            match self.peek().token_type {
                Struct | Fn | Let | Const | For | If | Loop | While | Return | Continue => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }
}