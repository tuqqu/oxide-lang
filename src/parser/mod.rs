use std::result;

use crate::error_token;
use crate::lexer::token::{Token, TokenType};
use crate::parser::expr::Expr::{
    GetPropExpr, IntLiteralExpr, SetIndexExpr, SetPropExpr, TypeCastExpr, VecIndexExpr,
};
use crate::parser::expr::{
    CallStruct, EnumDecl, FnSignatureDecl, GetProp, GetStaticProp, ImplDecl, IntLiteral, Lambda,
    Match, MatchArm, ParamList, SelfStatic, Self_, SetIndex, SetProp, StructDecl, TraitDecl,
    TypeCast, VecIndex, Vec_,
};
use crate::parser::valtype::{FnType, ValType};

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
pub mod valtype;

pub type Result<T> = result::Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    constructors: Vec<String>,
    current: usize,
    loop_depth: usize,
    fn_depth: usize,
    err: bool,
    current_impl_target: Option<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            constructors: vec![],
            current: 0,
            loop_depth: 0,
            fn_depth: 0,
            err: false,
            current_impl_target: None,
        }
    }

    pub fn parse(&mut self) -> result::Result<Vec<Stmt>, ()> {
        let mut stmts: Vec<Stmt> = Vec::<Stmt>::new();

        while !self.at_end() {
            let decl = self.decl_stmt();
            if let Some(decl) = decl {
                stmts.push(decl)
            }
        }

        if self.err {
            Err(())
        } else {
            Ok(stmts)
        }
    }

    fn decl_stmt(&mut self) -> Option<Stmt> {
        if self.match_token(TokenType::Let) {
            match self.var_decl() {
                Ok(var_decl) => Some(var_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.match_token(TokenType::Const) {
            match self.const_decl() {
                Ok(const_decl) => Some(const_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.check(TokenType::Fn) && self.check_next(TokenType::Identifier) {
            self.consume(TokenType::Fn, "Keyword \"fn\" expected")
                .unwrap();

            match self.fn_decl() {
                Ok(fn_decl) => Some(fn_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.match_token(TokenType::Enum) {
            match self.enum_decl() {
                Ok(enum_decl) => Some(enum_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.match_token(TokenType::Struct) {
            match self.struct_decl() {
                Ok(struct_decl) => Some(struct_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.match_token(TokenType::Impl) {
            match self.impl_decl() {
                Ok(impl_decl) => Some(impl_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else if self.match_token(TokenType::Trait) {
            match self.trait_decl() {
                Ok(trait_decl) => Some(trait_decl),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        } else {
            match self.any_stmt() {
                Ok(stmt) => Some(stmt),
                Err(p_err) => {
                    self.on_error(p_err);
                    None
                }
            }
        }
    }

    /// Parses variable mutable and immutable declaration syntax,
    /// as well as variable type and initializer.
    fn var_decl(&mut self) -> Result<Stmt> {
        let mutable: bool = self.match_token(TokenType::Mut);
        let name: Token = self.consume(TokenType::Identifier, "Variable name expected")?;

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
            "Semicolon \";\" expected after variable declaration",
        )?;
        let var_decl_stmt = Stmt::Let(VarDecl::new(name, Box::new(init), mutable, v_type));

        Ok(var_decl_stmt)
    }

    /// Struct properties have their own syntax which is different from usual vars,
    /// so they must be handled separately
    fn prop_decl(&mut self) -> Result<VarDecl> {
        let name: Token = self.consume(TokenType::Identifier, "Property name expected")?;

        self.consume(
            TokenType::Colon,
            "Colon \":\" expected after after prop declaration",
        )?;

        let v_type = self.type_decl()?;
        let var_decl = VarDecl::new(name, Box::new(None), true, Some(v_type));

        Ok(var_decl)
    }

    /// Type declaration for anything: variable, argument,
    /// function return type, struct field.
    fn type_decl(&mut self) -> Result<ValType> {
        self.consume_type()
    }

    /// Parses standalone constant declaration statement.
    fn const_decl(&mut self) -> Result<Stmt> {
        let const_decl_stmt = Stmt::Const(self.const_decl_inner()?);

        Ok(const_decl_stmt)
    }

    /// Used by both constant declaration and struct const declaration.
    /// Returns the inner ConstDecl struct, which represents the declaration itself,
    /// without it being a statement
    fn const_decl_inner(&mut self) -> Result<ConstDecl> {
        let name: Token = self.consume(TokenType::Identifier, "Constant name expected")?;

        let init = if self.match_token(TokenType::Equal) {
            if let Some(expr) = self.scalar_expr() {
                expr
            } else {
                return Err(ParseError::new(
                    &self.tokens[self.current],
                    "Constant must be initialized with a scalar value only",
                ));
            }
        } else {
            return Err(ParseError::new(
                &self.tokens[self.current],
                "Constant must be initialized",
            ));
        };

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after const declaration",
        )?;

        let const_decl = ConstDecl::new(name, Box::new(init));

        Ok(const_decl)
    }

    /// Parses the standalone function declaration statement.
    fn fn_decl(&mut self) -> Result<Stmt> {
        let (fn_decl, _) = self.fn_decl_inner(false)?;
        let fn_decl_stmt = Stmt::Fn(fn_decl);

        Ok(fn_decl_stmt)
    }

    /// Used by both function declaration and struct method declaration.
    /// Returns the inner FnDecl struct, which represents the declaration itself,
    /// without it being a statement
    fn fn_decl_inner(&mut self, expect_method: bool) -> Result<(FnDecl, bool)> {
        let name: Token = self.consume(TokenType::Identifier, "Function name expected")?;
        let (lambda, is_method) = self.lambda_expr(expect_method)?; // FIXME: rethink, maybe we need different methods for fn/method

        let lambda = match lambda {
            Expr::FnExpr(l) => l,
            _ => {
                return Err(ParseError::new(
                    &self.tokens[self.current],
                    "Unexpected expression type",
                ));
            }
        };

        let fn_decl = FnDecl::new(name, lambda);

        Ok((fn_decl, is_method))
    }

    /// Parses function signature. Used in trait declarations.
    fn fn_signature_decl(&mut self, expect_method: bool) -> Result<(FnSignatureDecl, bool)> {
        let name = self.consume(TokenType::Identifier, "Function name expected")?;
        let (params, instance_method) = self.param_list(expect_method)?;
        let ret_type = self.return_type()?;
        let fn_signature_decl = FnSignatureDecl::new(name, params, ret_type);

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after function signature declaration",
        )?;

        Ok((fn_signature_decl, instance_method))
    }

    /// Parses function type.
    fn fn_type(&mut self) -> Result<FnType> {
        let v_type_token = self.consume(TokenType::Fn, "\"fn\" expected in function type")?;

        self.consume(
            TokenType::LeftParen,
            "Parenthesis \"(\" expected before parameter list in function type",
        )?;

        let mut param_types: Vec<ValType> = vec![];

        if !self.check(TokenType::RightParen) {
            loop {
                let param_type = self.consume_type()?;
                param_types.push(param_type);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            "Parenthesis \")\" expected after parameter list in function type",
        )?;

        let ret_type = self.return_type()?;
        let fn_type = FnType::new(Some(v_type_token), param_types, Box::new(ret_type));

        Ok(fn_type)
    }

    /// Parses the enum declaration statement.
    fn enum_decl(&mut self) -> Result<Stmt> {
        let name: Token = self.consume(TokenType::Identifier, "Enum name expected")?;
        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected after enum name",
        )?;

        let mut values = vec![];

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            if self.check(TokenType::Identifier) {
                let enum_val = self.consume(TokenType::Identifier, "Expected enum value.")?;

                if values.contains(&enum_val) {
                    return Err(ParseError::new(
                        &enum_val,
                        &format!(
                            "Enum cannot have multiple identical values \"{}\"",
                            enum_val.lexeme
                        ),
                    ));
                }

                values.push(enum_val);

                if !self.check(TokenType::Comma) {
                    break;
                } else {
                    self.consume(
                        TokenType::Comma,
                        "Comma \",\" expected after enum value declaration",
                    )?;
                }
            } else {
                return Err(ParseError::new(
                    &self.tokens[self.current],
                    &format!("Unexpected token \"{}\"", self.peek().lexeme),
                ));
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after enum body",
        )?;

        self.constructors.push(name.lexeme.clone());
        let enum_decl_stmt = Stmt::Enum(EnumDecl::new(name, values));

        Ok(enum_decl_stmt)
    }

    /// Parses the struct declaration statement as well as all its properties
    /// whether they are `pub` and their type
    fn struct_decl(&mut self) -> Result<Stmt> {
        let name: Token = self.consume(TokenType::Identifier, "Struct name expected")?;
        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected after struct name",
        )?;

        let mut props = vec![];

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            let public = self.consume_pub()?;

            if self.check(TokenType::Identifier) {
                match self.prop_decl() {
                    Ok(var_decl) => {
                        props.push((var_decl, public));
                    }
                    Err(p_err) => {
                        self.on_error(p_err);
                    }
                };
                if !self.check(TokenType::Comma) {
                    break;
                } else {
                    self.consume(
                        TokenType::Comma,
                        "Comma \",\" expected after property declaration",
                    )?;
                }
            } else {
                return Err(ParseError::new(
                    &self.tokens[self.current],
                    &format!("Unexpected token \"{}\"", self.peek().lexeme),
                ));
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after struct body",
        )?;

        self.constructors.push(name.lexeme.clone());
        let struct_decl_stmt = Stmt::Struct(StructDecl::new(name, props));

        Ok(struct_decl_stmt)
    }

    /// Parses the struct implementation block:
    /// Struct instance methods, static methods, constants, implementation target
    /// Future scope: add types
    fn impl_decl(&mut self) -> Result<Stmt> {
        let impl_name: Token =
            self.consume(TokenType::Identifier, "Implementation target name expected")?;

        let for_name = if self.match_token(TokenType::For) {
            let name = self.consume(TokenType::Identifier, "Struct or enum name expected")?;
            Some(name)
        } else {
            None
        };

        self.current_impl_target = Some(impl_name.lexeme.clone());

        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected after impl",
        )?;

        // static methods
        let mut fns = vec![];
        // instance methods
        let mut methods = vec![];
        let mut consts = vec![];

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            let public = if for_name.is_some() {
                if self.check(TokenType::Pub) {
                    return Err(ParseError::new(
                        self.peek(),
                        "Trait methods must not be preceded with \"pub\", as they are always public")
                    );
                }

                true
            } else {
                self.consume_pub()?
            };

            if self.match_token(TokenType::Fn) {
                match self.fn_decl_inner(true) {
                    Ok((fn_decl, false)) => {
                        fns.push((fn_decl, public));
                    }
                    Ok((fn_decl, true)) => {
                        methods.push((fn_decl, public));
                    }
                    Err(p_err) => {
                        self.on_error(p_err);
                    }
                };
            } else if self.match_token(TokenType::Const) {
                match self.const_decl_inner() {
                    Ok(const_decl) => {
                        consts.push((const_decl, public));
                    }
                    Err(p_err) => {
                        self.on_error(p_err);
                    }
                };
            } else {
                return Err(ParseError::new(self.peek(), "Unexpected token"));
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after impl body",
        )?;

        let impl_decl_stmt = Stmt::Impl(ImplDecl::new(impl_name, for_name, methods, fns, consts));
        self.current_impl_target = None;

        Ok(impl_decl_stmt)
    }

    /// Parses the trait declaration block:
    /// instance methods
    /// Future scope: add default implementations
    /// Future scope: add static methods
    /// Future scope: add constants
    /// Future scope: add types
    fn trait_decl(&mut self) -> Result<Stmt> {
        let name: Token = self.consume(TokenType::Identifier, "Trait name expected")?;

        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected before trait body",
        )?;

        // instance methods
        let mut method_signs = vec![];

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            if self.match_token(TokenType::Fn) {
                let (sign, instance_method) = self.fn_signature_decl(true)?;

                // FIXME: add support for statics
                if !instance_method {
                    return Err(ParseError::new(
                        self.peek(),
                        "Static methods are not supported in traits",
                    ));
                }

                method_signs.push(sign);
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after trait body",
        )?;

        let trait_decl_stmt = Stmt::Trait(TraitDecl::new(name, method_signs));

        Ok(trait_decl_stmt)
    }

    /// Parses match expressions.
    /// Since match is an expression, it must end with a semicolon.
    fn match_expr(&mut self) -> Result<Expr> {
        let token = self.previous().clone();
        let expr = Box::new(self.any_expr()?);
        self.consume(
            TokenType::LeftCurlyBrace,
            "Curly brace \"{\" expected after match",
        )?;
        let mut branches = vec![];

        loop {
            let br_expr = Box::new(self.any_expr()?);
            self.consume(TokenType::FatArrow, "Arrow \"=>\" in after match branch")?;
            let br_body = Box::new(self.any_expr()?);
            branches.push(MatchArm::new(br_expr, br_body));

            if !self.match_token(TokenType::Comma) {
                break;
            }

            if self.check(TokenType::RightCurlyBrace) {
                break;
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after match body",
        )?;
        // FIXME: add default branch handling
        let match_expr = Expr::MatchExpr(Match::new(token, expr, branches, None));

        Ok(match_expr)
    }

    // FIXME: rethink "expect_method". Do we need Lambda? Shouldn't we use trait FnDecl and its impls instead
    fn lambda_expr(&mut self, expect_method: bool) -> Result<(Expr, bool)> {
        self.fn_depth += 1;

        let (params, instance_method) = self.param_list(expect_method)?;
        let ret_type = self.return_type()?;
        let body = self.block_stmt()?;

        let body = if let BlockStmt(block) = body {
            block
        } else {
            return Err(ParseError::new(self.peek(), "Function body error"));
        };

        let lambda_expr = Expr::FnExpr(Lambda::new(params, ret_type, body.stmts));

        self.fn_depth -= 1;

        Ok((lambda_expr, instance_method))
    }

    /// Parses parameter list in function declaration, expressions and signatures.
    fn param_list(&mut self, expect_method: bool) -> Result<(ParamList, bool)> {
        self.consume(
            TokenType::LeftParen,
            "Parenthesis \"(\" expected before parameter list",
        )?;

        let mut params: Vec<(Token, ValType, bool)> = vec![];
        let mut instance_method: bool = false;

        if !self.check(TokenType::RightParen) {
            loop {
                if self.check(TokenType::Self_) {
                    if !expect_method {
                        return Err(ParseError::new(
                            self.peek(),
                            "Only methods can have \"self\" in parameter list",
                        ));
                    } else if !params.is_empty() {
                        return Err(ParseError::new(
                            self.peek(),
                            &format!(
                                "\"self\" must be at index 0 in parameter list, got \"{}\"",
                                params.len() - 1
                            ),
                        ));
                    }

                    instance_method = true;
                    self.advance();
                } else {
                    if params.len() > FnDecl::MAX_ARGS {
                        return Err(ParseError::new(
                            self.peek(),
                            &format!("Cannot have more than {} arguments", FnDecl::MAX_ARGS),
                        ));
                    }

                    let mutable = self.check(TokenType::Mut);
                    if mutable {
                        self.advance();
                    }

                    let id =
                        self.consume(TokenType::Identifier, "Expected argument of a function")?;

                    let v_type = if self.match_token(TokenType::Colon) {
                        self.type_decl()?
                    } else {
                        return Err(ParseError::new(
                            &id,
                            &format!(
                                "Function argument \"{}\" must be explicitly typed",
                                id.lexeme
                            ),
                        ));
                    };

                    params.push((id, v_type, mutable));
                }

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            "Parenthesis \")\" expected after parameter list",
        )?;

        Ok((params, instance_method))
    }

    /// Parses return type. If no type is present, assumes it is `nil` value.
    fn return_type(&mut self) -> Result<ValType> {
        if self.match_token(TokenType::Arrow) {
            self.type_decl()
        } else {
            Ok(ValType::Nil)
        }
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
            "Semicolon \";\" expected after loop condition",
        )?;

        let inc = Box::new(if self.check(TokenType::LeftCurlyBrace) {
            Expr::EmptyExpr
        } else {
            match self.any_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    self.loop_depth -= 1;
                    return Err(e);
                }
            }
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
            return Err(ParseError::new(
                &self.previous(),
                "Must be inside a loop to use \"break\"",
            ));
        }

        self.consume(TokenType::Semicolon, "Semicolon \";\" expected after break")?;

        Ok(Stmt::Break)
    }

    fn continue_stmt(&mut self) -> Result<Stmt> {
        if self.loop_depth == 0 {
            return Err(ParseError::new(
                &self.previous(),
                "Must be inside a loop to use \"continue\"",
            ));
        }

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after continue",
        )?;

        Ok(Stmt::Continue)
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        if 0 == self.fn_depth {
            return Err(ParseError::new(
                &self.previous(),
                "Must be inside a function to use \"return\"",
            ));
        }

        let token = self.previous().clone();
        let expr = if !self.check(TokenType::Semicolon) {
            self.any_expr()?
        } else {
            Expr::EmptyExpr
        };

        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after return",
        )?;

        Ok(Stmt::Return(Return::new(token, Box::new(expr))))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.any_expr()?;
        self.consume(
            TokenType::Semicolon,
            "Semicolon \";\" expected after expression",
        )?;
        let expr_stmt = Stmt::Expr(expr);

        Ok(expr_stmt)
    }

    fn block_stmt(&mut self) -> Result<Stmt> {
        if self.match_token(TokenType::LeftCurlyBrace) {
            let block_stmt = BlockStmt(Block::new(self.block()?));

            return Ok(block_stmt);
        }

        Err(ParseError::new(
            &self.tokens[self.current],
            "Block statement expected",
        ))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::<Stmt>::new();

        while !self.check(TokenType::RightCurlyBrace) && !self.at_end() {
            if let Some(decl) = self.decl_stmt() {
                stmts.push(decl)
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after block",
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
            TokenType::BitwiseAndEqual,
            TokenType::BitwiseOrEqual,
            TokenType::BitwiseXorEqual,
        ]) {
            let operator: Token = self.previous().clone();
            let expr_val = self.assign_expr()?;

            return match &expr {
                VariableExpr(variable) => Ok(AssignmentExpr(Assignment::new(
                    variable.name.clone(),
                    operator,
                    Box::new(expr_val),
                ))),
                GetPropExpr(get_prop) => Ok(SetPropExpr(SetProp::new(
                    get_prop.name.clone(),
                    get_prop.prop_name.clone(),
                    operator,
                    Box::new(expr_val),
                ))),
                VecIndexExpr(vec_indx) => Ok(SetIndexExpr(SetIndex::new(
                    vec_indx.callee.clone(),
                    vec_indx.index.clone(),
                    operator,
                    Box::new(expr_val),
                ))),
                _ => Err(ParseError::new(&operator, "Invalid assignment target")),
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
        let mut expr = self.bitwise_expr()?;

        while self.match_tokens(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.bitwise_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn bitwise_expr(&mut self) -> Result<Expr> {
        let mut expr = self.sum_expr()?;

        while self.match_tokens(vec![
            TokenType::BitwiseAnd,
            TokenType::BitwiseOr,
            TokenType::BitwiseXor,
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
        let mut expr = self.as_expr()?;

        while self.match_tokens(vec![
            TokenType::Asterisk,
            TokenType::Slash,
            TokenType::Modulus,
        ]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.as_expr()?;
            expr = BinaryExpr(Binary::new(Box::new(expr), Box::new(right), operator));
        }

        Ok(expr)
    }

    fn as_expr(&mut self) -> Result<Expr> {
        let mut expr = self.unary_expr()?;

        while self.match_token(TokenType::As) {
            let operator: Token = self.previous().clone();
            let to_type: ValType = self.consume_type()?;
            expr = TypeCastExpr(TypeCast::new(Box::new(expr), to_type, operator));
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

        if self.constructors.contains(&self.previous().lexeme.clone()) {
            if let VariableExpr(_) = expr {
                if self.match_token(TokenType::LeftCurlyBrace) {
                    expr = self.finish_struct_call_expr(expr)?
                }
            }
        }

        // Check for: Self {...
        if self.previous().token_type == TokenType::SelfStatic
            && self.match_token(TokenType::LeftCurlyBrace)
        {
            expr = self.finish_struct_call_expr(expr)?;
        }

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_call_expr(expr)?;
            } else if self.match_token(TokenType::LeftBracket) {
                expr = self.finish_vec_index_expr(expr)?;
            } else if self.match_token(TokenType::Dot) {
                let name = self.consume(
                    TokenType::Identifier,
                    "Property or method name expected after \".\"",
                )?;
                expr = Expr::GetPropExpr(GetProp::new(Box::new(expr), name));
            } else if self.match_token(TokenType::ColonColon) {
                let name = self.consume(
                    TokenType::Identifier,
                    "Constant or static method name expected after \"::\"",
                )?;
                expr = Expr::GetStaticExpr(GetStaticProp::new(Box::new(expr), name));
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
                    return Err(ParseError::new(
                        self.peek(),
                        &format!("Cannot have more than {} arguments", FnDecl::MAX_ARGS),
                    ));
                }

                args.push(self.any_expr()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            "Parenthesis \")\" expected after arguments",
        )?;

        let call = Expr::CallExpr(Call::new(Box::new(callee), args));

        Ok(call)
    }

    fn finish_struct_call_expr(&mut self, callee: Expr) -> Result<Expr> {
        let mut args = vec![];

        loop {
            if args.len() >= FnDecl::MAX_ARGS {
                return Err(ParseError::new(
                    self.peek(),
                    &format!("Cannot have more than {} arguments", FnDecl::MAX_ARGS),
                ));
            }

            if self.check(TokenType::RightCurlyBrace) {
                break;
            }

            let prop = self.consume(TokenType::Identifier, "Property name expected")?;
            self.consume(TokenType::Colon, "Colon \":\" expected after after prop")?;

            args.push((prop, self.any_expr()?));

            if !self.match_token(TokenType::Comma) {
                break;
            }
        }

        self.consume(
            TokenType::RightCurlyBrace,
            "Curly brace \"}\" expected after arguments",
        )?;

        let call = Expr::CallStructExpr(CallStruct::new(Box::new(callee), args));

        Ok(call)
    }

    fn finish_vec_index_expr(&mut self, callee: Expr) -> Result<Expr> {
        let index_expr = self.any_expr()?;

        self.consume(TokenType::RightBracket, "Bracket \"]\" expected")?;

        let call = Expr::VecIndexExpr(VecIndex::new(Box::new(callee), Box::new(index_expr)));

        Ok(call)
    }

    fn vec_expr(&mut self) -> Result<Expr> {
        let token = self.previous().clone();
        let g_type = self.consume_generic_types(1, 1)?;

        self.consume(TokenType::LeftBracket, "Bracket \"[\" expected after vec")?;

        let mut values = vec![];

        loop {
            if self.check(TokenType::RightBracket) {
                break;
            }

            values.push(self.any_expr()?);

            if !self.match_token(TokenType::Comma) {
                break;
            }
        }

        self.consume(TokenType::RightBracket, "Bracket \"]\" expected after vec")?;

        let val_type = if g_type.first().is_some() {
            Some(g_type.first().unwrap().clone())
        } else {
            None
        };

        let call = Expr::VecExpr(Vec_::new(values, val_type, token));

        Ok(call)
    }

    fn map_expr(&mut self) -> Result<Expr> {
        // FIXME: add map type
        unimplemented!()
    }

    fn primary_expr(&mut self) -> Result<Expr> {
        if let Some(expr) = self.scalar_expr() {
            return Ok(expr);
        }

        if self.match_token(TokenType::Self_) {
            return Ok(Expr::SelfExpr(Self_::new(self.previous().clone())));
        }

        if self.match_token(TokenType::SelfStatic) {
            return Ok(Expr::SelfStaticExpr(SelfStatic::new(
                self.previous().clone(),
            )));
        }

        if self.match_token(TokenType::Identifier) {
            return Ok(VariableExpr(Variable::new(self.previous().clone())));
        }

        if self.match_token(TokenType::Fn) {
            let (lambda, _) = self.lambda_expr(false)?;
            return Ok(lambda);
        }

        if self.match_token(TokenType::Match) {
            return self.match_expr();
        }

        if self.match_token(TokenType::Vec) {
            return self.vec_expr();
        }

        if self.match_token(TokenType::Map) {
            return self.map_expr();
        }

        if self.match_token(TokenType::LeftParen) {
            let expr = self.any_expr()?;
            self.consume(
                TokenType::RightParen,
                "Parenthesis \")\" expected after expression",
            )?;
            let grouping_expr = GroupingExpr(Grouping::new(Box::new(expr)));

            return Ok(grouping_expr);
        }

        if self.check(TokenType::Semicolon) {
            return Ok(EmptyExpr);
        }

        Err(ParseError::new(self.peek(), "Expression expected"))
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

    fn consume_type(&mut self) -> Result<ValType> {
        use TokenType::*;

        if self.check(Num)
            || self.check(Float)
            || self.check(Int)
            || self.check(Nil)
            || self.check(Str)
            || self.check(Bool)
            || self.check(Map)
            || self.check(Any)
            || self.check(Identifier)
        {
            let v_type_token = self.advance().clone();
            let v_type = ValType::try_from_token(&v_type_token, None);
            return match v_type {
                Some(v_type) => Ok(v_type),
                None => Err(ParseError::new(&v_type_token, "Unrecognised type")),
            };
        }

        if self.check(Vec) {
            let v_type_token = self.advance().clone();
            let generics = self.consume_generic_types(1, 1)?;
            let generics = if generics.is_empty() {
                None
            } else {
                Some(generics)
            };

            let v_type = ValType::try_from_token(&v_type_token, generics);
            return match v_type {
                Some(v_type) => Ok(v_type),
                None => Err(ParseError::new(&v_type_token, "Unrecognised type")),
            };
        }

        if self.check(SelfStatic) {
            return if let Some(name) = self.current_impl_target.clone() {
                self.advance();
                Ok(ValType::Instance(name))
            } else {
                Err(ParseError::new(
                    &self.advance().clone(),
                    "Type \"Self\" can be used inside \"impl\" blocks only",
                ))
            };
        }

        if self.check(Fn) {
            let fn_type = self.fn_type()?;
            return Ok(ValType::Fn(fn_type));
        }

        Err(ParseError::new(
            self.peek(),
            &format!("Expected type, got \"{}\"", self.peek().lexeme),
        ))
    }

    fn consume_generic_types(&mut self, min: usize, max: usize) -> Result<Vec<ValType>> {
        if !self.check(TokenType::Less) {
            return Ok(vec![]);
        }

        self.consume(
            TokenType::Less,
            "Bracket \"<\" expected before generics type declaration",
        )?;

        let mut generics = vec![];

        loop {
            if self.check(TokenType::Greater) {
                break;
            }

            let val_type = self.consume_type()?;

            generics.push(val_type);

            if generics.len() > max {
                return Err(ParseError::new(
                    &self.peek(),
                    &format!("Too many generic types, expected only {}", max),
                ));
            }

            if !self.match_token(TokenType::Comma) {
                break;
            }
        }

        if generics.len() < min {
            return Err(ParseError::new(
                &self.peek(),
                &format!("Too few generic types, expected at least \"{}\"", min),
            ));
        }

        self.consume(
            TokenType::Greater,
            "Bracket \">\" expected after generics type declaration",
        )?;

        Ok(generics)
    }

    fn consume_pub(&mut self) -> Result<bool> {
        if self.check(TokenType::Pub) {
            self.consume(TokenType::Pub, "Expected \"pub\"")?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self, t_type: TokenType, msg: &str) -> Result<Token> {
        if !self.check(t_type) {
            return Err(ParseError::new(self.peek(), msg));
        }

        Ok(self.advance().clone())
    }

    fn on_error(&mut self, p_err: ParseError) {
        self.err = true;
        error_token(&p_err.token, &p_err.msg);
        self.try_to_recover();
    }

    fn try_to_recover(&mut self) {
        use TokenType::*;

        self.advance();

        while !self.at_end() {
            if self.previous().token_type == Semicolon {
                return;
            }

            match self.peek().token_type {
                Trait | Enum | Struct | Fn | Impl | Let | Const | For | If | Loop | While
                | Match | Return | Continue => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    token: Token,
    msg: String,
}

impl ParseError {
    fn new(token: &Token, msg: &str) -> Self {
        Self {
            token: token.clone(),
            msg: msg.to_string(),
        }
    }
}
