use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;
use std::{mem, result};

use oxide_parser::expr::{
    Assignment, Binary, BoolLiteral, Call, CallStruct, Expr, FloatLiteral, GetProp, GetStaticProp,
    Grouping, IntLiteral, Lambda, Match, NilLiteral, SelfStatic, Self_, SetIndex, SetProp,
    StrLiteral, TypeCast, Unary, Variable, VecIndex, Vec_,
};
use oxide_parser::stmt::{
    Block, ConstDecl, EnumDecl, FnDecl, ForIn, If, ImplDecl, Loop, Return, Stmt, StructDecl,
    TraitDecl, VarDecl,
};
use oxide_parser::valtype::{TYPE_BOOL, TYPE_FLOAT, TYPE_FN, TYPE_INT, TYPE_STRUCT, TYPE_VEC};
use oxide_parser::{Ast, Token, TokenType, ValType};

use crate::env::{construct_static_name, Env, EnvVal, ResolvableName, ValuableName};
use crate::error::RuntimeError;
use crate::val::{
    try_vtype_from_val, vtype_conforms_val, Callable, Function, PropFuncVal, StmtVal,
    StructCallable, StructInstance, Val, VecInstance,
};
use crate::{env, StreamProvider};

pub type InterpretedResult<T> = result::Result<T, RuntimeError>;

#[derive(Clone)]
enum Mode {
    /// Any statements allowed on the top-level.
    TopLevel,
    /// Only item declaration allowed on the top-level.
    /// Execution starts from "main".
    EntryPoint(Option<Box<env::Function>>),
}

pub struct Interpreter {
    streams: Box<dyn StreamProvider>,
    env: Rc<RefCell<Env>>,
    mode: Mode,
    args: Vec<Val>,
    /// Superglobals. Not used atm.
    #[allow(dead_code)]
    glob: Rc<RefCell<Env>>,
}

impl Interpreter {
    const ENTRY_POINT: &'static str = "main";

    /// Returns an interpreter instance.
    pub(crate) fn new(stdlib: Env, streams: Box<dyn StreamProvider>, argv: &[String]) -> Self {
        let mut args = Vec::<Val>::with_capacity(argv.len());
        for arg in argv {
            args.push(Val::Str(arg.clone()));
        }

        let glob = Rc::new(RefCell::new(stdlib));
        let env = Rc::clone(&glob);

        Self {
            streams,
            glob,
            env,
            args,
            mode: Mode::EntryPoint(None),
        }
    }

    /// Interpret statements.
    pub fn interpret(&mut self, ast: &Ast) -> InterpretedResult<Val> {
        if ast.top_level() {
            self.mode = Mode::TopLevel;
        }

        for stmt in ast.tree() {
            self.evaluate_stmt(stmt)?;
        }

        match self.mode.clone() {
            Mode::EntryPoint(f) => {
                let f = f.map(|f| *f);
                match f {
                    Some(f) => {
                        self.mode = Mode::TopLevel;
                        // FIXME: improve return value support
                        let result = self.call_expr(f.val(), &[])?;
                        Ok(result)
                    }
                    _ => Err(RuntimeError::Script(
                        None,
                        String::from(&format!(
                            "No entry-point \"{}\" function found",
                            Self::ENTRY_POINT
                        )),
                    )),
                }
            }
            Mode::TopLevel => Ok(Val::Nil),
        }
    }

    fn eval_enum_stmt(&mut self, stmt: &EnumDecl) -> InterpretedResult<StmtVal> {
        self.check_name(stmt.name())?;

        for (val, name) in stmt.vals().iter().enumerate() {
            let val_name_t = name.clone();
            let name_t = stmt.name().clone();
            let name = name_t.lexeme.clone();
            let val_name = val_name_t.lexeme.clone();

            self.env.borrow_mut().define_enum_value(env::EnumValue::new(
                val_name_t,
                Val::EnumValue(name, val_name, val),
                name_t,
            ));
        }

        let enum_ = env::Enum::new(stmt.name().clone(), Val::Enum(stmt.name().clone()));

        self.env.borrow_mut().define_enum(enum_);

        Ok(StmtVal::None)
    }

    fn eval_struct_stmt(&mut self, stmt: &StructDecl) -> InterpretedResult<StmtVal> {
        self.check_name(stmt.name())?;

        let decl = stmt.clone();
        let struct_ = env::Struct::new(
            stmt.name().lexeme.clone(),
            Val::Struct(
                stmt.name().clone(),
                *StructCallable::new_boxed(
                    decl.props().len(),
                    Arc::new(move |inter, args| {
                        let impls = inter.env.borrow_mut().get_impls(decl.name());
                        let impls = if let Some(impls) = impls {
                            impls
                        } else {
                            vec![]
                        };

                        let instance = StructInstance::new(decl.clone(), impls);

                        for (prop, param) in args {
                            let mut instance_borrowed = instance.borrow_mut();
                            if let Some((_, v_type, public)) =
                                instance_borrowed.props().get(&prop.lexeme)
                            {
                                let public = *public;
                                let v_type = v_type.clone();
                                if vtype_conforms_val(&v_type, param) {
                                    instance_borrowed.props_mut().insert(
                                        prop.lexeme.clone(),
                                        (param.clone(), v_type, public),
                                    );
                                } else {
                                    return Err(RuntimeError::Type(
                                        Some(prop.clone()),
                                        format!(
                                            "Expected argument \"{}\" of type \"{}\"",
                                            v_type,
                                            param.get_type()
                                        ),
                                    ));
                                }
                            } else {
                                return Err(RuntimeError::Definition(
                                    Some(prop.clone()),
                                    format!("Unknown property name \"{}\"", prop.lexeme),
                                ));
                            }
                        }

                        let instance = Val::StructInstance(instance);

                        Ok(instance)
                    }),
                ),
            ),
        );

        self.env.borrow_mut().define_struct(struct_);

        Ok(StmtVal::None)
    }

    fn eval_impl_stmt(&mut self, stmt: &ImplDecl) -> InterpretedResult<StmtVal> {
        let decl = stmt.clone();

        let (impl_name, trait_name) = if let Some(for_name) = decl.for_name() {
            let trait_ = self.env.borrow_mut().get(decl.impl_name())?;
            let trait_ = trait_.borrow_mut().deref().clone();

            match trait_ {
                EnvVal::Trait(t) => {
                    for signature in t.methods() {
                        // FIXME: improve traversing
                        let mut found = false;
                        for (method, _pub) in decl.methods() {
                            if method.name() == signature.name() {
                                if method.lambda().ret_type() != signature.ret_type()
                                    || method.lambda().params() != signature.params()
                                {
                                    return Err(RuntimeError::Definition(
                                        Some(method.name().clone()),
                                        format!(
                                            "Mismatched signature of method \"{}\"",
                                            method.name().lexeme
                                        ),
                                    ));
                                }

                                found = true;
                            }
                        }

                        if !found {
                            return Err(RuntimeError::Definition(
                                Some(signature.name().clone()),
                                format!(
                                    "Method \"{}\" must be implemented",
                                    signature.name().lexeme
                                ),
                            ));
                        }
                    }
                }
                _ => {
                    return Err(RuntimeError::Definition(
                        Some(for_name.clone()),
                        String::from("Expected trait name"),
                    ))
                }
            }

            (
                for_name.clone().lexeme,
                Some(decl.impl_name().lexeme.clone()),
            )
        } else {
            (decl.impl_name().lexeme.clone(), None)
        };

        for (const_decl, pub_) in decl.consts() {
            let val = self.evaluate(const_decl.init())?;
            Self::validate_const_type(const_decl, &val)?;

            self.env
                .borrow_mut()
                .define_constant(env::Constant::with_struct(
                    const_decl.name().clone(),
                    val,
                    (stmt.impl_name().clone(), *pub_),
                ))?;
        }

        for (fn_, pub_) in decl.fns() {
            let val = self.eval_fn_expr(
                fn_.lambda(),
                None,
                Some(stmt.impl_name().lexeme.clone()),
                false,
            );

            self.env
                .borrow_mut()
                .define_function(env::Function::with_struct(
                    fn_.name().clone(),
                    val,
                    (stmt.impl_name().clone(), *pub_),
                ))?;
        }

        for (fn_, pub_) in decl.methods() {
            let val = self.eval_fn_expr(
                fn_.lambda(),
                None,
                Some(stmt.impl_name().lexeme.clone()),
                true,
            );

            let static_name = if let Some(name) = stmt.for_name().clone() {
                name
            } else {
                stmt.impl_name().clone()
            };

            self.env
                .borrow_mut()
                .define_function(env::Function::with_struct(
                    fn_.name().clone(),
                    val,
                    (static_name, *pub_),
                ))?;
        }

        self.env.borrow_mut().define_impl(env::Impl::new(
            impl_name,
            trait_name,
            decl.methods().to_vec(),
            decl.fns().to_vec(),
            decl.consts().to_vec(),
        ))?;

        Ok(StmtVal::None)
    }

    fn eval_trait_stmt(&mut self, stmt: &TraitDecl) -> InterpretedResult<StmtVal> {
        self.check_name(stmt.name())?;

        self.env.borrow_mut().define_trait(env::Trait::new(
            stmt.name().lexeme.clone(),
            stmt.method_signs().to_vec(),
            Val::Trait(stmt.name().clone()),
        ));

        Ok(StmtVal::None)
    }

    fn eval_expr_stmt(&mut self, expr: &Expr) -> InterpretedResult<StmtVal> {
        self.evaluate(expr)?;

        Ok(StmtVal::None)
    }

    fn eval_var_stmt(&mut self, stmt: &VarDecl) -> InterpretedResult<StmtVal> {
        let val: Val = match stmt.init() {
            Some(init) => self.evaluate(init)?,
            None => Val::Uninit,
        };

        let v_type: ValType;

        if stmt.v_type().is_some() {
            v_type = stmt.v_type().clone().unwrap();

            if !vtype_conforms_val(&v_type, &val) {
                return Err(RuntimeError::Type(
                    Some(stmt.name().clone()),
                    format!(
                        "Trying to initialise variable of type \"{}\" with value of type \"{}\"",
                        v_type,
                        val.get_type()
                    ),
                ));
            }
        } else {
            v_type = match try_vtype_from_val(&val) {
                Some(v_type) => v_type,
                None => {
                    return Err(RuntimeError::Type(
                        Some(stmt.name().clone()),
                        format!(
                            "Unrecognised value type in initialisation \"{}\"",
                            val.get_type()
                        ),
                    ));
                }
            }
        }

        self.env.borrow_mut().define_variable(env::Variable::new(
            stmt.name().lexeme.clone(),
            val,
            stmt.mutable(),
            v_type,
        ));

        Ok(StmtVal::None)
    }

    fn eval_const_stmt(&mut self, stmt: &ConstDecl) -> InterpretedResult<StmtVal> {
        let val: Val = self.evaluate(stmt.init())?;
        Self::validate_const_type(stmt, &val)?;

        self.env
            .borrow_mut()
            .define_constant(env::Constant::without_struct(stmt.name().clone(), val))?;

        Ok(StmtVal::None)
    }

    fn eval_if_stmt(&mut self, stmt: &If) -> InterpretedResult<StmtVal> {
        if Self::is_true(&self.evaluate(stmt.condition())?)? {
            self.evaluate_stmt(stmt.then_stmt())
        } else {
            self.evaluate_stmt(stmt.else_stmt())
        }
    }

    fn eval_loop_stmt(&mut self, stmt: &Loop) -> InterpretedResult<StmtVal> {
        while Self::is_true(&self.evaluate(stmt.condition())?)? {
            let v = self.evaluate_stmt(stmt.body())?;

            match v {
                StmtVal::None => {}
                StmtVal::Continue => {
                    self.evaluate(stmt.inc())?;
                    continue;
                }
                StmtVal::Break => {
                    return Ok(StmtVal::None);
                }
                stmt_val @ StmtVal::Return(_) => {
                    return Ok(stmt_val);
                }
            }

            self.evaluate(stmt.inc())?;
        }

        Ok(StmtVal::None)
    }

    fn eval_for_in_stmt(&mut self, stmt: &ForIn) -> InterpretedResult<StmtVal> {
        let iter = &self.evaluate(stmt.iter())?;
        match iter {
            Val::VecInstance(v) => {
                let env = Rc::new(RefCell::new(Env::with_enclosing(self.env.clone())));
                env.borrow_mut().define_variable(env::Variable::new(
                    stmt.iter_value().lexeme.clone(),
                    Val::Uninit,
                    true,
                    v.borrow().val_type().clone(),
                ));

                if stmt.index_value().is_some() {
                    env.borrow_mut().define_variable(env::Variable::new(
                        stmt.index_value().clone().unwrap().lexeme,
                        Val::Uninit,
                        true,
                        ValType::Int,
                    ));
                }

                for (pos, val) in v.borrow().vals().iter().enumerate() {
                    env.borrow_mut().assign(stmt.iter_value().clone(), val)?;

                    if stmt.index_value().is_some() {
                        env.borrow_mut()
                            .assign(stmt.index_value().clone().unwrap(), &Val::Int(pos as isize))?;
                    }

                    let v = self.evaluate_block(stmt.body(), Some(env.clone()))?;

                    match v {
                        StmtVal::None => {}
                        StmtVal::Continue => {
                            continue;
                        }
                        StmtVal::Break => {
                            return Ok(StmtVal::None);
                        }
                        stmt_val @ StmtVal::Return(_) => {
                            return Ok(stmt_val);
                        }
                    }
                }
            }
            v => {
                return Err(RuntimeError::Type(
                    Some(stmt.iter_value().clone()),
                    format!(
                        "Trying to iterate over a non-iterable value of type \"{}\"",
                        v.get_type()
                    ),
                ))
            }
        }

        Ok(StmtVal::None)
    }

    fn eval_match_expr(&mut self, expr: &Match) -> InterpretedResult<Val> {
        let cond: Val = self.evaluate(expr.expr())?;

        for arm in expr.arms() {
            let br_cond: Val = self.evaluate(arm.expr())?;
            if let Val::Bool(true) = Val::equal(&cond, &br_cond, expr.keyword())? {
                return self.evaluate(arm.body());
            }
        }

        Err(RuntimeError::Runtime(
            expr.keyword().clone(),
            String::from("Match expression must be exhaustive"),
        ))
    }

    fn eval_var_expr(&mut self, expr: &Variable) -> InterpretedResult<Val> {
        let env_val = self.env.borrow_mut().get(expr.name())?;
        let env_val = env_val.borrow_mut();

        use EnvVal::*;

        match env_val.deref() {
            Function(f) => Ok(f.val().clone()),
            Constant(c) => Ok(c.val().clone()),
            Variable(v) => {
                let val = v.val();
                if let Val::Uninit = val {
                    Err(RuntimeError::Runtime(
                        expr.name().clone(),
                        format!(
                            "Trying to access an uninitialized variable \"{}\"",
                            expr.name().lexeme
                        ),
                    ))
                } else {
                    Ok(val)
                }
            }
            Enum(e) => Ok(e.val().clone()),
            EnumValue(e) => Ok(e.val().clone()),
            Struct(s) => Ok(s.val().clone()),
            Trait(t) => Ok(t.val().clone()),
        }
    }

    fn eval_assign_expr(&mut self, expr: &Assignment) -> InterpretedResult<Val> {
        let val = self.evaluate(expr.expr())?;
        let val = match expr.operator().token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual
            | TokenType::BitwiseAndEqual
            | TokenType::BitwiseOrEqual
            | TokenType::BitwiseXorEqual => {
                let env_val = self.env.borrow_mut().get(expr.name())?;
                let env_val = env_val.borrow_mut();
                match env_val.deref() {
                    EnvVal::Variable(v) => {
                        Self::evaluate_two_operands(expr.operator(), &v.val(), &val)?
                    }
                    _ => {
                        return Err(RuntimeError::Operator(
                            expr.name().clone(),
                            format!(
                                "Operator \"{}\" can be used only with a variables",
                                expr.operator().lexeme
                            ),
                        ))
                    }
                }
            }
            _ => {
                return Err(RuntimeError::Operator(
                    expr.operator().clone(),
                    str::to_string("Unrecognised token in an assignment expression"),
                ))
            }
        };

        self.env.borrow_mut().assign(expr.name().clone(), &val)?;

        Ok(val)
    }

    fn eval_fn_stmt(&mut self, fn_decl: &FnDecl) -> InterpretedResult<StmtVal> {
        let fn_val = self.eval_fn_expr(fn_decl.lambda(), None, None, false);
        let func: env::Function = env::Function::without_struct(fn_decl.name().clone(), fn_val);

        if fn_decl.name().lexeme == Self::ENTRY_POINT {
            if let Mode::EntryPoint(entry_point) = &self.mode {
                match entry_point {
                    None => self.mode = Mode::EntryPoint(Some(Box::new(func))),
                    Some(_) => {
                        return Err(RuntimeError::Script(
                            Some(fn_decl.name().clone()),
                            format!(
                                "Entry-point function \"{}\" cannot be declared twice.",
                                Self::ENTRY_POINT
                            ),
                        ))
                    }
                }
            }
        } else {
            self.env.borrow_mut().define_function(func)?;
        }

        Ok(StmtVal::None)
    }

    fn eval_fn_expr(
        &mut self,
        expr: &Lambda,
        self_: Option<Rc<RefCell<StructInstance>>>,
        self_static: Option<String>,
        self_argument: bool,
    ) -> Val {
        let copy = Rc::clone(&self.env);

        let func = Function::new(
            expr.clone(),
            Rc::new(RefCell::new(Env::with_enclosing(copy))),
        );

        let ret_type = func.lambda().ret_type().clone();
        let mut param_types: Vec<ValType> = func
            .lambda()
            .params()
            .to_vec()
            .into_iter()
            .map(|(_, vt, _)| vt)
            .collect();

        let mut self_type = None;

        if self_argument {
            assert!(
                !self_static.is_none(),
                "Function cannot have \"self\" as an argument without \"self_static\" being set"
            );

            self_type = Some(ValType::Instance(self_static.clone().unwrap()));
            param_types.insert(0, self_type.clone().unwrap());
        }

        Val::Callable(*Callable::new_boxed(
            param_types,
            ret_type,
            Arc::new(move |inter, args| {
                let copy = Rc::clone(func.env());
                let glob = Rc::new(RefCell::new(Env::with_enclosing(copy)));
                let mut env = Env::with_enclosing(glob);

                if self_.is_some() {
                    let cur_instance = self_.clone().unwrap();
                    env.define_static_bind(cur_instance.borrow().struct_name().to_string());
                    env.define_self(cur_instance);
                }

                if self_static.is_some() {
                    let static_bind = self_static.clone().unwrap();
                    env.define_static_bind(static_bind);
                }

                if self_argument {
                    assert!(!(self_static.is_none() || self_type.is_none()), "Function cannot have \"self\" as an argument without \"self_static\" or \"self_type\" being set");

                    let cur_instance = args[0].clone();
                    let self_type = self_type.as_ref().unwrap();
                    if !vtype_conforms_val(self_type, &cur_instance) {
                        return Err(RuntimeError::Type(
                            None,
                            format!(
                                "Expected argument \"{}\" of type \"{}\", got \"{}\"",
                                0,
                                self_type,
                                cur_instance.get_type()
                            ),
                        ));
                    }

                    let cur_instance = if let Val::StructInstance(cur_instance) = cur_instance {
                        cur_instance
                    } else {
                        panic!("Expected to have StructInstance as a current instance value.");
                    };

                    env.define_static_bind(cur_instance.borrow().struct_name().to_string());
                    env.define_self(cur_instance);
                }

                for (i, param) in func.lambda().params().iter().enumerate() {
                    let arg_index = if self_argument { i + 1 } else { i };
                    let arg = args[arg_index].clone();

                    if !vtype_conforms_val(&param.1, &arg) {
                        return Err(RuntimeError::Type(
                            Some(param.0.clone()),
                            format!(
                                "Expected argument \"{}\" of type \"{}\", got \"{}\"",
                                arg_index,
                                param.1,
                                arg.get_type()
                            ),
                        ));
                    }

                    let var = env::Variable::new(
                        param.0.lexeme.clone(),
                        args[arg_index].clone(),
                        param.2,
                        param.1.clone(),
                    );

                    env.define_variable(var);
                }

                let new_env = Rc::new(RefCell::new(env));
                let stmt_val = inter.evaluate_block(func.lambda().body(), Some(new_env))?;

                let val = match stmt_val {
                    StmtVal::None => Val::Nil,
                    StmtVal::Return(val) => val,
                    _ => {
                        return Err(RuntimeError::Script(
                            None,
                            str::to_string("Unknown statement value"),
                        ))
                    }
                };

                if vtype_conforms_val(func.lambda().ret_type(), &val) {
                    Ok(val)
                } else {
                    Err(RuntimeError::Type(
                        None,
                        format!(
                            "Function must return \"{}\", got \"{}\"",
                            func.lambda().ret_type(),
                            val.get_type()
                        ),
                    ))
                }
            }),
        ))
    }

    fn eval_block_stmt(&mut self, stmt: &Block) -> InterpretedResult<StmtVal> {
        self.evaluate_block(stmt, None)
    }

    fn eval_break_stmt(&mut self) -> StmtVal {
        StmtVal::Break
    }

    fn eval_continue_stmt(&mut self) -> StmtVal {
        StmtVal::Continue
    }

    fn eval_return_stmt(&mut self, expr: &Return) -> InterpretedResult<StmtVal> {
        let val = match expr.expr() {
            Expr::EmptyExpr => Val::Nil,
            expr => self.evaluate(expr)?,
        };

        Ok(StmtVal::Return(val))
    }

    fn eval_unary_expr(&mut self, expr: &Unary) -> InterpretedResult<Val> {
        let un_expr: Val = self.evaluate(expr.expr())?;

        let val = match expr.operator().token_type {
            TokenType::Bang => match un_expr {
                Val::Bool(b) => Val::Bool(!b),
                val => {
                    return Err(RuntimeError::Type(
                        Some(expr.operator().clone()),
                        format!(
                            "Expected \"{}\" value, got \"{}\"",
                            TYPE_BOOL,
                            val.get_type()
                        ),
                    ))
                }
            },
            TokenType::Minus => match un_expr {
                Val::Float(n) => Val::Float(-n),
                Val::Int(n) => Val::Int(-n),
                val => {
                    return Err(RuntimeError::Type(
                        Some(expr.operator().clone()),
                        format!(
                            "Expected \"{}\" or \"{}\" value, got \"{}\"",
                            TYPE_INT,
                            TYPE_FLOAT,
                            val.get_type()
                        ),
                    ))
                }
            },
            _ => {
                return Err(RuntimeError::Runtime(
                    expr.operator().clone(),
                    format!("Unknown unary \"{}\" operator", expr.operator().lexeme),
                ))
            }
        };

        Ok(val)
    }

    fn eval_call_expr(&mut self, expr: &Call) -> InterpretedResult<Val> {
        let callee = self.evaluate(expr.callee())?;

        self.call_expr(&callee, expr.args())
    }

    fn call_expr(&mut self, callee: &Val, args: &[Expr]) -> InterpretedResult<Val> {
        match callee {
            Val::Callable(callee) => {
                let mut eval_args = vec![];
                for arg in args {
                    eval_args.push(self.evaluate(arg)?);
                }

                if eval_args.len() != callee.arity() {
                    return Err(RuntimeError::Definition(
                        None,
                        format!(
                            "Expected {} arguments but got {}",
                            callee.arity(),
                            eval_args.len()
                        ),
                    ));
                }

                (callee.call())(self, &eval_args)
            }
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Callable value must be of type \"{}\", got \"{}\"",
                    TYPE_FN,
                    callee.get_type()
                ),
            )),
        }
    }

    fn eval_self_static_expr(&mut self, expr: &SelfStatic) -> InterpretedResult<Val> {
        let self_static = self.env.borrow_mut().get_static_bind();

        match self_static {
            Some(s) => {
                let self_token = Token::from_token(expr.self_static(), s);
                let struct_ = self.env.borrow_mut().get(&self_token)?;
                let struct_ = struct_.borrow_mut().deref().clone();

                match struct_ {
                    EnvVal::Struct(s) => Ok(s.val().clone()),
                    EnvVal::Enum(e) => Ok(e.val().clone()),
                    _ => Err(RuntimeError::Runtime(
                        expr.self_static().clone(),
                        str::to_string("Wrong static bind target"),
                    )),
                }
            }
            None => Err(RuntimeError::Runtime(
                expr.self_static().clone(),
                str::to_string("Value \"Self\" can be used in methods only"),
            )),
        }
    }

    fn eval_self_expr(&mut self, expr: &Self_) -> InterpretedResult<Val> {
        let self_ = self.env.borrow_mut().get_self();
        let self_ = match self_ {
            Some(s) => s,
            None => {
                return Err(RuntimeError::Runtime(
                    expr.self_().clone(),
                    str::to_string("Value \"self\" can be used in non-static methods only"),
                ))
            }
        };

        Ok(Val::StructInstance(self_))
    }

    fn eval_call_struct_expr(&mut self, expr: &CallStruct) -> InterpretedResult<Val> {
        let callee = self.evaluate(expr.callee())?;
        match callee {
            Val::Struct(token, callee) => {
                let mut args = vec![];

                for (token, arg) in expr.args() {
                    args.push((token.clone(), self.evaluate(arg)?));
                }

                if args.len() != callee.arity() {
                    return Err(RuntimeError::Definition(
                        Some(token),
                        format!(
                            "Expected {} arguments but got {}",
                            callee.arity(),
                            args.len()
                        ),
                    ));
                }

                (callee.call())(self, &args)
            }
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Callable value must be of type \"{}\", got \"{}\"",
                    TYPE_STRUCT,
                    callee.get_type()
                ),
            )),
        }
    }

    fn eval_vec_expr(&mut self, expr: &Vec_) -> InterpretedResult<Val> {
        let mut values = vec![];
        let val_type = if expr.vals().is_empty() {
            expr.val_type().clone().unwrap_or(ValType::Any)
        } else if expr.val_type().is_some() {
            let val_type = expr.val_type().clone().unwrap();
            for val_expr in expr.vals() {
                let val = self.evaluate(val_expr)?;
                if !vtype_conforms_val(&val_type, &val) {
                    return Err(RuntimeError::Type(
                        Some(expr.token().clone()),
                        format!(
                            "Expected values of type \"{}\", got \"{}\"",
                            val_type,
                            try_vtype_from_val(&val).unwrap()
                        ),
                    ));
                }

                values.push(val);
            }

            val_type
        } else {
            let mut val_type = None;
            for val_expr in expr.vals() {
                let val = self.evaluate(val_expr)?;

                if val_type.is_none() {
                    val_type = try_vtype_from_val(&val);
                } else if !vtype_conforms_val(&val_type.clone().unwrap(), &val) {
                    val_type = Some(ValType::Any);
                }

                values.push(val);
            }

            val_type.unwrap()
        };

        let vec_val = Val::VecInstance(Rc::new(RefCell::new(VecInstance::new(values, val_type))));

        Ok(vec_val)
    }

    fn eval_vec_index(&mut self, expr: &VecIndex) -> InterpretedResult<Val> {
        let val = self.evaluate(expr.callee())?;
        let pos = self.evaluate(expr.index())?;
        let pos = if let Val::Int(int) = pos {
            int as usize
        } else {
            return Err(RuntimeError::Type(
                None,
                format!(
                    "Values of type \"{}\" can have indices of type \"{}\", got \"{}\"",
                    TYPE_VEC,
                    TYPE_INT,
                    pos.get_type()
                ),
            ));
        };

        match val {
            Val::VecInstance(vec) => vec.borrow().get(pos),
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Indexing works with values of type \"{}\", got \"{}\"",
                    TYPE_VEC,
                    val.get_type()
                ),
            )),
        }
    }

    fn eval_get_static_prop_expr(&mut self, expr: &GetStaticProp) -> InterpretedResult<Val> {
        fn extract_static_value(
            name: &(impl ResolvableName + ValuableName),
            public_access: bool,
            token: Token,
            static_name: &str,
        ) -> InterpretedResult<Val> {
            if let Some((_name, pub_)) = name.for_target() {
                if StructInstance::can_access(*pub_, public_access) {
                    Ok(name.val().clone())
                } else {
                    Err(RuntimeError::Definition(
                        Some(token),
                        format!("Cannot access private static member \"{}\"", static_name),
                    ))
                }
            } else {
                panic!("Static access value must not be a standalone one.");
            }
        }

        let static_caller = self.evaluate(expr.name())?;

        match static_caller {
            Val::Struct(token, _) | Val::Enum(token) | Val::Trait(token) => {
                let static_name = construct_static_name(&token.lexeme, &expr.prop_name().lexeme);
                let public_access = self.is_public_static_access(token.lexeme.clone());
                let static_val = self
                    .env
                    .borrow_mut()
                    .get(&Token::from_token(&token, static_name.clone()))?;
                let env_val = static_val.borrow_mut();
                match env_val.deref() {
                    // FIXME: can it lead to a bug? this branch should be possible only in Enum case
                    EnvVal::EnumValue(e) => Ok(e.val().clone()),
                    EnvVal::Constant(c) => {
                        extract_static_value(c, public_access, token, &static_name)
                    }
                    EnvVal::Function(f) => {
                        extract_static_value(f, public_access, token, &static_name)
                    }
                    _ => Err(RuntimeError::Definition(
                        Some(token),
                        String::from("Unknown static access value"),
                    )),
                }
            }
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Unknown static callee type \"{}\"",
                    static_caller.get_type()
                ),
            )),
        }
    }

    fn eval_get_prop_expr(&mut self, expr: &GetProp) -> InterpretedResult<Val> {
        let instance = self.evaluate(expr.name())?;
        match instance {
            Val::StructInstance(i) => {
                let struct_name = i.borrow().struct_name().to_string();
                let public_access = self.is_public_access(struct_name);
                let val = i.borrow_mut().get_prop(expr.prop_name(), public_access)?;
                match val {
                    PropFuncVal::Prop(val) => Ok(val),
                    PropFuncVal::Func((func, self_, _pub)) => {
                        Ok(self.eval_fn_expr(&func, Some(self_), None, false))
                    }
                }
            }
            Val::VecInstance(vec) => VecInstance::get_method(expr.prop_name(), vec),
            // FIXME: add instance methods for enums
            _ => Err(RuntimeError::Type(
                None,
                format!("Must be a struct instance, got \"{}\"", instance.get_type()),
            )),
        }
    }

    fn eval_set_prop_expr(&mut self, expr: &SetProp) -> InterpretedResult<Val> {
        let instance = self.evaluate(expr.name())?;
        let instance = if let Val::StructInstance(i) = instance {
            i
        } else {
            return Err(RuntimeError::Type(
                None,
                str::to_string("Must be a struct instance"),
            ));
        };

        let val = self.evaluate(expr.expr())?;
        let val = match expr.operator().token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual
            | TokenType::BitwiseAndEqual
            | TokenType::BitwiseOrEqual
            | TokenType::BitwiseXorEqual => {
                let struct_name = instance.borrow().struct_name().to_string();
                let public_access = self.is_public_access(struct_name);

                let r_val = instance
                    .borrow_mut()
                    .get_prop(expr.prop_name(), public_access)?;
                let r_val = match r_val {
                    PropFuncVal::Prop(val) => val,
                    _ => {
                        return Err(RuntimeError::Definition(
                            Some(expr.operator().clone()),
                            str::to_string("Must be a property"),
                        ))
                    }
                };

                Self::evaluate_two_operands(expr.operator(), &val, &r_val)?
            }
            _ => {
                return Err(RuntimeError::Operator(
                    expr.operator().clone(),
                    str::to_string("Unrecognised token in an assignment expression"),
                ))
            }
        };

        let struct_name = instance.borrow().struct_name().to_string();
        let public_access = self.is_public_access(struct_name);
        let mut instance = instance.borrow_mut();

        instance.set_prop(expr.prop_name(), val.clone(), public_access)?;

        Ok(val)
    }

    fn eval_set_index_expr(&mut self, expr: &SetIndex) -> InterpretedResult<Val> {
        let vec = self.evaluate(expr.name())?;
        let vec = if let Val::VecInstance(v) = vec {
            v
        } else if let Val::Uninit = vec {
            return Err(RuntimeError::Runtime(
                expr.operator().clone(),
                str::to_string("Out of bounds"),
            ));
        } else {
            return Err(RuntimeError::Type(
                None,
                format!("Must be a vec instance, got \"{}\"", vec),
            ));
        };

        let index = self.evaluate(expr.index())?;
        let index = if let Val::Int(int) = index {
            int as usize
        } else {
            return Err(RuntimeError::Type(
                Some(expr.operator().clone()),
                format!(
                    "Values of type \"{}\" can have indices of type \"{}\", got \"{}\"",
                    TYPE_VEC,
                    TYPE_INT,
                    index.get_type()
                ),
            ));
        };

        let val = self.evaluate(expr.expr())?;
        let mut vec = vec.borrow_mut();

        if !vtype_conforms_val(vec.val_type(), &val) {
            return Err(RuntimeError::Type(
                Some(expr.operator().clone()),
                format!(
                    "Cannot assign value of type \"{}\" to a vector of type \"{}\"",
                    val.get_type(),
                    vec.val_type()
                ),
            ));
        }

        let val = match expr.operator().token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual
            | TokenType::BitwiseAndEqual
            | TokenType::BitwiseOrEqual
            | TokenType::BitwiseXorEqual => {
                let l_val = vec.get(index)?;
                Self::evaluate_two_operands(expr.operator(), &l_val, &val)?
            }
            _ => {
                return Err(RuntimeError::Operator(
                    expr.operator().clone(),
                    str::to_string("Unrecognised token in an assignment expression"),
                ))
            }
        };

        vec.set(index, val.clone())?;

        Ok(val)
    }

    fn eval_logical_binary_expr(&mut self, expr: &Binary) -> InterpretedResult<Val> {
        let left = self.evaluate(expr.left())?;
        if let Val::Bool(l_val) = left {
            if expr.operator().token_type == TokenType::LogicOr {
                if l_val {
                    return Ok(left);
                }
            } else if !l_val {
                return Ok(left);
            }
        } else {
            return Err(RuntimeError::Type(
                Some(expr.operator().clone()),
                format!(
                    "Only boolean values can be used in logical expressions, got \"{}\"",
                    left.get_type()
                ),
            ));
        }

        let right = self.evaluate(expr.right())?;
        if let Val::Bool(_) = right {
            Ok(right)
        } else {
            Err(RuntimeError::Type(
                Some(expr.operator().clone()),
                format!(
                    "Only boolean values can be used in logical expressions, got \"{}\"",
                    right.get_type()
                ),
            ))
        }
    }

    fn eval_type_cast_expr(&mut self, expr: &TypeCast) -> InterpretedResult<Val> {
        let left = self.evaluate(expr.left())?;
        let cast = left.cast_to(expr.to_type(), expr.operator())?;

        Ok(cast)
    }

    fn eval_binary_expr(&mut self, expr: &Binary) -> InterpretedResult<Val> {
        let left = self.evaluate(expr.left())?;
        let right = self.evaluate(expr.right())?;
        let val = Self::evaluate_two_operands(expr.operator(), &left, &right)?;

        Ok(val)
    }

    fn evaluate_two_operands(operator: &Token, lhs: &Val, rhs: &Val) -> InterpretedResult<Val> {
        match operator.token_type {
            //equality
            TokenType::EqualEqual => Val::equal(lhs, rhs, operator),
            TokenType::BangEqual => Val::not_equal(lhs, rhs, operator),
            // comparison
            TokenType::Greater => Val::greater(lhs, rhs, operator),
            TokenType::GreaterEqual => Val::greater_equal(lhs, rhs, operator),
            TokenType::Less => Val::less(lhs, rhs, operator),
            TokenType::LessEqual => Val::less_equal(lhs, rhs, operator),
            // math
            TokenType::Minus | TokenType::MinusEqual => Val::subtract(lhs, rhs, operator),
            TokenType::Plus | TokenType::PlusEqual => Val::add(lhs, rhs, operator),
            TokenType::Slash | TokenType::SlashEqual => Val::divide(lhs, rhs, operator),
            TokenType::Modulus | TokenType::ModulusEqual => Val::modulus(lhs, rhs, operator),
            TokenType::Asterisk | TokenType::AsteriskEqual => Val::multiply(lhs, rhs, operator),
            // bitwise
            TokenType::BitwiseAnd | TokenType::BitwiseAndEqual => {
                Val::bitwise_and(lhs, rhs, operator)
            }
            TokenType::BitwiseOr | TokenType::BitwiseOrEqual => Val::bitwise_or(lhs, rhs, operator),
            TokenType::BitwiseXor | TokenType::BitwiseXorEqual => {
                Val::bitwise_xor(lhs, rhs, operator)
            }
            TokenType::DotDot => Val::range(lhs, rhs, operator),
            TokenType::DotDotEqual => Val::range_equal(lhs, rhs, operator),
            _ => Err(RuntimeError::Operator(
                operator.clone(),
                format!("Unknown binary operator \"{}\"", operator.lexeme),
            )),
        }
    }

    fn eval_grouping_expr(&mut self, expr: &Grouping) -> InterpretedResult<Val> {
        self.evaluate(expr.expr())
    }

    fn eval_nil_literal(&self, _expr: &NilLiteral) -> Val {
        Val::Nil
    }

    fn eval_bool_literal(&self, expr: &BoolLiteral) -> Val {
        Val::Bool(expr.0)
    }

    fn eval_int_literal(&self, expr: &IntLiteral) -> Val {
        Val::Int(expr.0)
    }

    fn eval_float_literal(&self, expr: &FloatLiteral) -> Val {
        Val::Float(expr.0)
    }

    fn eval_str_literal(&self, expr: &StrLiteral) -> Val {
        Val::Str(expr.0.clone())
    }

    fn evaluate(&mut self, expr: &Expr) -> InterpretedResult<Val> {
        use Expr::*;
        let val = match expr {
            EmptyExpr => Val::Nil,
            NilLiteralExpr(literal) => self.eval_nil_literal(literal),
            BoolLiteralExpr(literal) => self.eval_bool_literal(literal),
            IntLiteralExpr(literal) => self.eval_int_literal(literal),
            FloatLiteralExpr(literal) => self.eval_float_literal(literal),
            StrLiteralExpr(literal) => self.eval_str_literal(literal),
            UnaryExpr(unary) => self.eval_unary_expr(unary)?,
            SelfStaticExpr(self_static) => self.eval_self_static_expr(self_static)?,
            SelfExpr(self_) => self.eval_self_expr(self_)?,
            CallExpr(call) => self.eval_call_expr(call)?,
            CallStructExpr(call_struct) => self.eval_call_struct_expr(call_struct)?,
            VecExpr(call_vec) => self.eval_vec_expr(call_vec)?,
            VecIndexExpr(vec_index) => self.eval_vec_index(vec_index)?,
            GetStaticExpr(get_static_prop) => self.eval_get_static_prop_expr(get_static_prop)?,
            GetPropExpr(get_prop) => self.eval_get_prop_expr(get_prop)?,
            SetPropExpr(set_prop) => self.eval_set_prop_expr(set_prop)?,
            SetIndexExpr(set_index) => self.eval_set_index_expr(set_index)?,
            BinaryExpr(binary) => self.eval_binary_expr(binary)?,
            LogicalBinaryExpr(l_binary) => self.eval_logical_binary_expr(l_binary)?,
            TypeCastExpr(type_cast) => self.eval_type_cast_expr(type_cast)?,
            GroupingExpr(grouping) => self.eval_grouping_expr(grouping)?,
            VariableExpr(variable) => self.eval_var_expr(variable)?,
            AssignmentExpr(assignment) => self.eval_assign_expr(assignment)?,
            FnExpr(lambda) => self.eval_fn_expr(lambda, None, None, false),
            MatchExpr(match_expr) => self.eval_match_expr(match_expr)?,
        };

        Ok(val)
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt) -> InterpretedResult<StmtVal> {
        match (&self.mode, stmt) {
            (Mode::EntryPoint(_), Stmt::Expr(_))
            | (Mode::EntryPoint(_), Stmt::Let(_))
            | (Mode::EntryPoint(_), Stmt::Break)
            | (Mode::EntryPoint(_), Stmt::Continue)
            | (Mode::EntryPoint(_), Stmt::Return(_))
            | (Mode::EntryPoint(_), Stmt::BlockStmt(_))
            | (Mode::EntryPoint(_), Stmt::IfStmt(_))
            | (Mode::EntryPoint(_), Stmt::LoopStmt(_))
            | (Mode::EntryPoint(_), Stmt::ForInStmt(_)) => Err(RuntimeError::Script(
                None,
                String::from(
                    "Only item (\"const\", \"impl\", \"struct\", \"fn\", \"enum\", \
                    \"trait\") declarations are allowed on the top-level",
                ),
            )),

            (Mode::TopLevel, Stmt::Expr(expr_stmt)) => self.eval_expr_stmt(expr_stmt),
            (Mode::TopLevel, Stmt::Let(var_decl)) => self.eval_var_stmt(var_decl),
            (Mode::TopLevel, Stmt::Break) => Ok(self.eval_break_stmt()),
            (Mode::TopLevel, Stmt::Continue) => Ok(self.eval_continue_stmt()),
            (Mode::TopLevel, Stmt::Return(return_stmt)) => self.eval_return_stmt(return_stmt),
            (Mode::TopLevel, Stmt::BlockStmt(block)) => self.eval_block_stmt(block),
            (Mode::TopLevel, Stmt::IfStmt(if_stmt)) => self.eval_if_stmt(if_stmt),
            (Mode::TopLevel, Stmt::LoopStmt(loop_stmt)) => self.eval_loop_stmt(loop_stmt),
            (Mode::TopLevel, Stmt::ForInStmt(for_in_stmt)) => self.eval_for_in_stmt(for_in_stmt),

            (_, Stmt::Const(const_decl)) => self.eval_const_stmt(const_decl),
            (_, Stmt::Fn(f_decl)) => self.eval_fn_stmt(f_decl),
            (_, Stmt::Enum(enum_decl)) => self.eval_enum_stmt(enum_decl),
            (_, Stmt::Struct(struct_decl)) => self.eval_struct_stmt(struct_decl),
            (_, Stmt::Impl(impl_decl)) => self.eval_impl_stmt(impl_decl),
            (_, Stmt::Trait(trait_decl)) => self.eval_trait_stmt(trait_decl),
        }
    }

    fn evaluate_block(
        &mut self,
        block: &Block,
        env: Option<Rc<RefCell<Env>>>,
    ) -> InterpretedResult<StmtVal> {
        let new_env = if let Some(env) = env {
            env
        } else {
            Rc::new(RefCell::new(Env::with_enclosing(Rc::clone(&self.env))))
        };

        let old_env = mem::replace(&mut self.env, new_env);

        for stmt in block.stmts() {
            let stmt_val = self.evaluate_stmt(stmt)?;
            match &stmt_val {
                StmtVal::None => {}
                StmtVal::Break | StmtVal::Continue | StmtVal::Return(_) => {
                    self.env = old_env;
                    return Ok(stmt_val);
                }
            }
        }

        self.env = old_env;

        Ok(StmtVal::None)
    }

    fn is_public_access(&self, struct_name: String) -> bool {
        if let Some(self_) = self.env.borrow().get_self() {
            self_.borrow().struct_name() != struct_name
        } else {
            self.is_public_static_access(struct_name)
        }
    }

    fn is_public_static_access(&self, struct_name: String) -> bool {
        if let Some(static_bind) = self.env.borrow().get_static_bind() {
            static_bind != struct_name
        } else {
            true
        }
    }

    fn check_name(&self, name: &Token) -> InterpretedResult<()> {
        if self.env.borrow().has_definition(&name.lexeme) {
            Err(RuntimeError::Definition(
                Some(name.clone()),
                format!("Name \"{}\" is already in use", &name.lexeme),
            ))
        } else {
            Ok(())
        }
    }

    fn is_true(val: &Val) -> InterpretedResult<bool> {
        match val {
            Val::Bool(true) => Ok(true),
            Val::Bool(false) => Ok(false),
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Trying to evaluate value of type \"{}\" as boolean",
                    val.get_type()
                ),
            )),
        }
    }

    fn validate_const_type(const_decl: &ConstDecl, val: &Val) -> InterpretedResult<()> {
        if let Some(v_type) = const_decl.v_type() {
            if !vtype_conforms_val(v_type, val) {
                return Err(RuntimeError::Type(
                    Some(const_decl.name().clone()),
                    format!(
                        "Constant type \"{}\" and init value type \"{}\" mismatch",
                        v_type,
                        val.get_type()
                    ),
                ));
            }
        }

        Ok(())
    }

    pub(crate) fn args(&self) -> &[Val] {
        &self.args
    }

    #[allow(clippy::borrowed_box)]
    pub(crate) fn streams(&self) -> &Box<dyn StreamProvider> {
        &self.streams
    }
}
