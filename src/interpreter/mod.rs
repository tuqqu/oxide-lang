use std::cell::RefCell;
use std::io::{Read, Write};
use std::rc::Rc;
use std::sync::Arc;
use std::{mem, result};

use crate::interpreter::val::{PropFuncVal, StructCallable};
use crate::interpreter::val::{StructInstance, VecInstance};

use crate::lexer::token::{Pos, Token, TokenType};
use crate::parser::expr::{
    Assignment, Binary, Block, BoolLiteral, Call, CallStruct, ConstDecl, EnumDecl, Expr,
    FloatLiteral, FnDecl, GetProp, GetStaticProp, Grouping, If, ImplDecl, IntLiteral, Lambda, Loop,
    Match, NilLiteral, Return, SelfStatic, Self_, SetIndex, SetProp, Stmt, StrLiteral, StructDecl,
    Unary, VarDecl, Variable, VecIndex, Vec_,
};

use crate::parser::valtype::{
    ValType, TYPE_BOOL, TYPE_FN, TYPE_INT, TYPE_NUM, TYPE_STR, TYPE_STRUCT, TYPE_VEC,
};

use self::env::{Env, EnvVal};
use self::val::{Callable, Function, StmtVal, Val};
use crate::interpreter::env::construct_static_name;
use std::ops::Deref;

pub mod env;
pub mod stdlib;
pub mod val;

pub type Result<T> = result::Result<T, RuntimeError>;

pub struct Interpreter {
    pub stdout: Rc<RefCell<dyn Write>>,
    pub stderr: Rc<RefCell<dyn Write>>,
    pub stdin: Rc<RefCell<dyn Read>>,
    pub glob: Rc<RefCell<Env>>,
    pub env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new(
        stdlib: Env,
        stdout: Rc<RefCell<dyn Write>>,
        stderr: Rc<RefCell<dyn Write>>,
        stdin: Rc<RefCell<dyn Read>>,
    ) -> Self {
        let glob = Rc::new(RefCell::new(stdlib));
        let env = Rc::clone(&glob);

        Self {
            stdout,
            stderr,
            stdin,
            glob,
            env,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.evaluate_stmt(stmt)?;
        }

        Ok(())
    }

    fn eval_enum_stmt(&mut self, stmt: &EnumDecl) -> Result<StmtVal> {
        if self.env.borrow().has_definition(&stmt.name.lexeme) {
            return Err(RuntimeError::from_token(
                stmt.name.clone(),
                format!("Name '{}' is already in use", stmt.name.lexeme),
            ));
        }
        for (val, name) in stmt.vals.iter().enumerate() {
            let val_name_t = name.clone();
            let name_t = stmt.name.clone();
            let name = name_t.lexeme.clone();
            let val_name = val_name_t.lexeme.clone();

            self.env.borrow_mut().define_enum_value(env::EnumValue::new(
                val_name_t,
                Val::EnumValue(name, val_name, val),
                name_t,
            ));
        }

        let enum_ = env::Enum::new(stmt.name.clone(), Val::Enum(stmt.name.clone()));

        self.env.borrow_mut().define_enum(enum_);

        Ok(StmtVal::None)
    }

    fn eval_struct_stmt(&mut self, stmt: &StructDecl) -> Result<StmtVal> {
        if self.env.borrow().has_definition(&stmt.name.lexeme) {
            return Err(RuntimeError::from_token(
                stmt.name.clone(),
                format!("Name '{}' is already in use", stmt.name.lexeme),
            ));
        }
        let decl = stmt.clone();
        let struct_ = env::Struct::new(
            stmt.name.lexeme.clone(),
            Val::Struct(
                stmt.name.clone(),
                *StructCallable::new(
                    decl.props.len(),
                    Arc::new(move |inter, args| {
                        let impl_ = inter.env.borrow_mut().get_impl(&decl.name);
                        let instance = StructInstance::new(decl.clone(), impl_);

                        for (prop, param) in args {
                            let mut instance_borrowed = instance.borrow_mut();
                            if let Some((_, v_type, public)) =
                                instance_borrowed.props.get(&prop.lexeme)
                            {
                                let public = *public;
                                let v_type = v_type.clone();
                                if v_type.conforms(&param) {
                                    instance_borrowed.props.insert(
                                        prop.lexeme.clone(),
                                        (param.clone(), v_type, public),
                                    );
                                } else {
                                    return Err(RuntimeError::from_token(
                                        prop.clone(),
                                        format!(
                                            "Expected argument \"{}\" of type \"{}\"",
                                            v_type,
                                            param.get_type()
                                        ),
                                    ));
                                }
                            } else {
                                return Err(RuntimeError::from_token(
                                    prop.clone(),
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

    fn eval_impl_stmt(&mut self, stmt: &ImplDecl) -> Result<StmtVal> {
        let decl = stmt.clone();

        for (const_, pub_) in &decl.consts {
            let val: Val = self.evaluate(&const_.init)?;

            self.env
                .borrow_mut()
                .define_constant(env::Constant::with_struct(
                    const_.name.clone(),
                    val,
                    (stmt.for_struct.clone(), *pub_),
                ))?;
        }

        for (fn_, pub_) in &decl.fns {
            let val = self.eval_fn_expr(
                &fn_.lambda.clone(),
                None,
                Some(stmt.for_struct.lexeme.clone()),
            );

            self.env
                .borrow_mut()
                .define_function(env::Function::with_struct(
                    fn_.name.clone(),
                    val,
                    (stmt.for_struct.clone(), *pub_),
                ))?;
        }

        self.env.borrow_mut().define_impl(env::Impl::new(
            decl.for_struct.lexeme.clone(),
            decl.methods,
            decl.fns,
            decl.consts,
        ))?;

        Ok(StmtVal::None)
    }

    fn eval_expr_stmt(&mut self, expr: &Expr) -> Result<StmtVal> {
        self.evaluate(&expr)?;

        Ok(StmtVal::None)
    }

    fn eval_var_stmt(&mut self, stmt: &VarDecl) -> Result<StmtVal> {
        let val: Val = match &*stmt.init {
            Some(init) => self.evaluate(init)?,
            None => Val::Uninit,
        };

        let v_type: ValType;

        if stmt.v_type.is_some() {
            v_type = stmt.v_type.clone().unwrap();

            if !v_type.conforms(&val) {
                return Err(RuntimeError::from_token(
                    stmt.name.clone(),
                    format!(
                        "Trying to initialise variable of type \"{}\" with value of type \"{}\"",
                        v_type,
                        val.get_type()
                    ),
                ));
            }
        } else {
            v_type = match ValType::try_from_val(&val) {
                Some(v_type) => v_type,
                None => {
                    return Err(RuntimeError::from_token(
                        stmt.name.clone(),
                        format!(
                            "Unrecognised value type in initialisation \"{}\"",
                            val.get_type()
                        ),
                    ));
                }
            }
        }

        self.env.borrow_mut().define_variable(env::Variable::new(
            stmt.name.lexeme.clone(),
            val,
            stmt.mutable,
            v_type,
        ));

        Ok(StmtVal::None)
    }

    fn eval_const_stmt(&mut self, stmt: &ConstDecl) -> Result<StmtVal> {
        let val: Val = self.evaluate(&stmt.init)?;

        self.env
            .borrow_mut()
            .define_constant(env::Constant::without_struct(stmt.name.clone(), val))?;

        Ok(StmtVal::None)
    }

    fn eval_if_stmt(&mut self, stmt: &If) -> Result<StmtVal> {
        let truth: bool = Self::is_true(&self.evaluate(&stmt.condition)?)?;
        if truth {
            self.evaluate_stmt(&stmt.then_stmt)
        } else if let Some(else_stmt) = &stmt.else_stmt {
            self.evaluate_stmt(else_stmt)
        } else {
            Ok(StmtVal::None)
        }
    }

    fn eval_loop_stmt(&mut self, stmt: &Loop) -> Result<StmtVal> {
        while Self::is_true(&self.evaluate(&stmt.condition)?)? {
            let v = self.evaluate_stmt(&stmt.body)?;

            match v {
                StmtVal::None => {}
                StmtVal::Continue => {
                    self.evaluate(&stmt.inc)?;
                    continue;
                }
                StmtVal::Break => {
                    return Ok(StmtVal::None);
                }
                _ => {}
            }

            self.evaluate(&stmt.inc)?;
        }

        Ok(StmtVal::None)
    }

    fn eval_match_expr(&mut self, expr: &Match) -> Result<Val> {
        let cond: Val = self.evaluate(&expr.expr)?;

        for arm in &expr.arms {
            let br_cond: Val = self.evaluate(&arm.expr)?;
            if Val::equal(&cond, &br_cond) {
                return self.evaluate(&arm.body);
            }
        }

        Err(RuntimeError::from_token(
            expr.keyword.clone(),
            "Match expression must be exhaustive".to_string(),
        ))
    }

    fn eval_var_expr(&mut self, expr: &Variable) -> Result<Val> {
        let env_val = self.env.borrow_mut().get(expr.name.clone())?;
        let env_val = env_val.borrow_mut();

        use EnvVal::*;

        match env_val.deref() {
            NoValue => Err(RuntimeError::from_token(
                expr.name.clone(),
                format!(
                    "Trying to access uninitialized variable \"{}\"",
                    expr.name.lexeme
                ),
            )),
            Function(f) => Ok(f.val.clone()),
            Constant(c) => Ok(c.val.clone()),
            Variable(v) => Ok(v.val.clone()),
            Enum(e) => Ok(e.val.clone()),
            EnumValue(e) => Ok(e.val.clone()),
            Struct(s) => Ok(s.val.clone()),
        }
    }

    fn eval_assign_expr(&mut self, expr: &Assignment) -> Result<Val> {
        let val = self.evaluate(&expr.expr)?;
        let val = match expr.operator.token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual => {
                let env_val = self.env.borrow_mut().get(expr.name.clone())?;
                let env_val = env_val.borrow_mut();
                match env_val.deref() {
                    EnvVal::Variable(v) => {
                        Self::evaluate_two_operands(expr.operator.clone(), v.val.clone(), val)?
                    }
                    _ => {
                        return Err(RuntimeError::from_token(
                            expr.name.clone(),
                            format!(
                                "Operator \"{}\" can be used only with variables",
                                expr.operator.lexeme
                            ),
                        ))
                    }
                }
            }
            _ => {
                return Err(RuntimeError::from_token(
                    expr.operator.clone(),
                    "Unrecognised token in an assignment expression".to_string(),
                ))
            }
        };

        self.env.borrow_mut().assign(expr.name.clone(), &val)?;

        Ok(val)
    }

    fn eval_fn_stmt(&mut self, fn_decl: &FnDecl) -> Result<StmtVal> {
        let func: env::Function = env::Function::without_struct(
            fn_decl.name.clone(),
            self.eval_fn_expr(&fn_decl.lambda.clone(), None, None),
        );

        self.env.borrow_mut().define_function(func)?;

        Ok(StmtVal::None)
    }

    fn eval_fn_expr(
        &mut self,
        expr: &Lambda,
        self_: Option<Rc<RefCell<StructInstance>>>,
        self_static: Option<String>,
    ) -> Val {
        let copy = Rc::clone(&self.env);

        let func = Function::new(
            expr.clone(),
            Rc::new(RefCell::new(Env::with_enclosing(copy))),
        );

        Val::Callable(*Callable::new(
            func.param_size(),
            Arc::new(move |inter, args| {
                let copy = Rc::clone(&func.env);
                let glob = Rc::new(RefCell::new(Env::with_enclosing(copy)));
                let mut env = Env::with_enclosing(glob);

                if self_.is_some() {
                    let cur_instance = self_.clone().unwrap();
                    env.define_static_bind(cur_instance.borrow().struct_name.clone())?;
                    env.define_self(cur_instance)?;
                }

                if self_static.is_some() {
                    let static_bind = self_static.clone().unwrap();
                    env.define_static_bind(static_bind)?;
                }

                for (i, param) in func.lambda.params.iter().enumerate() {
                    let arg = args[i].clone();

                    if !param.1.conforms(&arg) {
                        return Err(RuntimeError::from_token(
                            param.0.clone(),
                            format!(
                                "Expected argument \"{}\" of type \"{}\", got \"{}\"",
                                i,
                                param.1,
                                arg.get_type()
                            ),
                        ));
                    }

                    let var = env::Variable::new(
                        param.0.lexeme.clone(),
                        args[i].clone(),
                        param.2,
                        param.1.clone(),
                    );

                    env.define_variable(var);
                }

                let newenv = Rc::new(RefCell::new(env));
                let stmt_val = inter.evaluate_block(&func.lambda.body, Some(newenv))?;

                let val = match stmt_val {
                    StmtVal::None => Val::Nil,
                    StmtVal::Return(val) => val,
                    _ => return Err(RuntimeError::new("Unknown statement value".to_string())),
                };

                if func.lambda.ret_type.conforms(&val) {
                    Ok(val)
                } else {
                    Err(RuntimeError::new(format!(
                        "Function must return \"{}\", got \"{}\"",
                        func.lambda.ret_type,
                        val.get_type()
                    )))
                }
            }),
        ))
    }

    fn eval_block_stmt(&mut self, stmt: &Block) -> Result<StmtVal> {
        self.evaluate_block(&stmt.stmts, None)
    }

    fn eval_break_stmt(&mut self) -> StmtVal {
        StmtVal::Break
    }

    fn eval_continue_stmt(&mut self) -> StmtVal {
        StmtVal::Continue
    }

    fn eval_return_stmt(&mut self, expr: &Return) -> Result<StmtVal> {
        let val = match &*expr.expr {
            Expr::EmptyExpr => Val::Nil,
            expr => self.evaluate(&expr)?,
        };

        Ok(StmtVal::Return(val))
    }

    fn eval_unary_expr(&mut self, expr: &Unary) -> Result<Val> {
        let un_expr: Val = self.evaluate(&expr.expr)?;

        let val = match expr.operator.token_type {
            TokenType::Bang => match un_expr {
                Val::Bool(b) => Val::Bool(!b),
                val => {
                    return Err(RuntimeError::from_token(
                        expr.operator.clone(),
                        format!("Expected \"bool\" value, got \"{}\"", val.get_type()),
                    ))
                }
            },
            TokenType::Minus => match un_expr {
                Val::Float(n) => Val::Float(-n),
                Val::Int(n) => Val::Int(-n),
                val => {
                    return Err(RuntimeError::from_token(
                        expr.operator.clone(),
                        format!("Expected \"num\" value, got \"{}\"", val.get_type()),
                    ))
                }
            },
            _ => {
                return Err(RuntimeError::from_token(
                    expr.operator.clone(),
                    format!("Unknown unary \"{}\"", expr.operator.lexeme),
                ))
            }
        };

        Ok(val)
    }

    fn eval_call_expr(&mut self, expr: &Call) -> Result<Val> {
        let callee = self.evaluate(&expr.callee)?;

        match callee {
            Val::Callable(callee) => {
                let mut args = vec![];
                for arg in &expr.args {
                    args.push(self.evaluate(&arg)?);
                }

                if args.len() != callee.arity {
                    return Err(RuntimeError::new(format!(
                        "Expected {} arguments but got {}",
                        callee.arity,
                        args.len()
                    )));
                }

                (callee.call)(self, &args)
            }
            _ => Err(RuntimeError::new(format!(
                "Callable value must be of type \"{}\", got \"{}\"",
                TYPE_FN,
                callee.get_type()
            ))),
        }
    }

    fn eval_self_static_expr(&mut self, expr: &SelfStatic) -> Result<Val> {
        let self_static = self.env.borrow_mut().get_static_bind();

        match self_static {
            Some(s) => {
                let struct_ = self.env.borrow_mut().get_by_str(&s, expr.self_static.pos)?;
                let struct_ = struct_.borrow_mut().deref().clone();

                if let EnvVal::Struct(s) = struct_ {
                    Ok(s.val)
                } else {
                    Err(RuntimeError::from_token(
                        expr.self_static.clone(),
                        "Wrong static bind target".to_string(),
                    ))
                }
            }
            None => Err(RuntimeError::from_token(
                expr.self_static.clone(),
                "Value \"Self\" can be used in methods only".to_string(),
            )),
        }
    }

    fn eval_self_expr(&mut self, expr: &Self_) -> Result<Val> {
        let self_ = self.env.borrow_mut().get_self();
        let self_ = match self_ {
            Some(s) => s,
            None => {
                return Err(RuntimeError::from_token(
                    expr.self_.clone(),
                    "Value \"self\" can be used in methods only".to_string(),
                ))
            }
        };

        Ok(Val::StructInstance(self_))
    }

    fn eval_call_struct_expr(&mut self, expr: &CallStruct) -> Result<Val> {
        let callee = self.evaluate(&expr.callee)?;

        match callee {
            Val::Struct(token, callee) => {
                let mut args = vec![];

                for (token, arg) in &expr.args {
                    args.push((token.clone(), self.evaluate(&arg)?));
                }

                if args.len() != callee.arity {
                    return Err(RuntimeError::from_token(
                        token,
                        format!("Expected {} arguments but got {}", callee.arity, args.len()),
                    ));
                }

                (callee.call)(self, &args)
            }
            _ => Err(RuntimeError::new(format!(
                "Callable value must be of type \"{}\", got \"{}\"",
                TYPE_STRUCT,
                callee.get_type()
            ))),
        }
    }

    fn eval_vec_expr(&mut self, expr: &Vec_) -> Result<Val> {
        let mut values = vec![];
        let val_type = if expr.vals.is_empty() {
            expr.val_type.clone().unwrap_or(ValType::Any)
        } else if expr.val_type.is_some() {
            let val_type = expr.val_type.clone().unwrap();
            for val_expr in &expr.vals {
                let val = self.evaluate(val_expr)?;
                if !val_type.conforms(&val) {
                    return Err(RuntimeError::from_token(
                        expr.token.clone(),
                        format!(
                            "Expected values of type \"{}\", got \"{}\"",
                            val_type,
                            ValType::try_from_val(&val).unwrap()
                        ),
                    ));
                }

                values.push(val);
            }

            val_type
        } else {
            let mut val_type = None;
            for val_expr in &expr.vals {
                let val = self.evaluate(val_expr)?;

                if val_type.is_none() {
                    val_type = ValType::try_from_val(&val);
                } else if !val_type.clone().unwrap().conforms(&val) {
                    val_type = Some(ValType::Any);
                }

                values.push(val);
            }

            val_type.unwrap()
        };

        let vec_val = Val::VecInstance(Rc::new(RefCell::new(VecInstance::new(values, val_type))));

        Ok(vec_val)
    }

    fn eval_vec_index(&mut self, expr: &VecIndex) -> Result<Val> {
        let val = self.evaluate(&expr.callee)?;
        let indx = self.evaluate(&expr.index)?;
        let indx = if let Val::Int(int) = indx {
            int as usize
        } else {
            return Err(RuntimeError::new(format!(
                "Values of type \"{}\" can have indices of type \"{}\", got \"{}\"",
                TYPE_VEC,
                TYPE_INT,
                indx.get_type()
            )));
        };

        match val {
            Val::VecInstance(vec) => vec.borrow_mut().get(indx),
            _ => Err(RuntimeError::new(format!(
                "Indexing works with values of type \"{}\", got \"{}\"",
                TYPE_VEC,
                val.get_type()
            ))),
        }
    }

    fn eval_get_static_prop_expr(&mut self, expr: &GetStaticProp) -> Result<Val> {
        let static_caller = self.evaluate(&expr.name)?;

        match static_caller {
            Val::Struct(token, _) => {
                let static_name = construct_static_name(&token.lexeme, &expr.prop_name.lexeme);
                let public_access = self.is_public_static_access(token.lexeme.clone());
                let static_val = self.env.borrow_mut().get_by_str(&static_name, token.pos)?;
                let env_val = static_val.borrow_mut();
                match env_val.deref() {
                    EnvVal::Constant(c) => {
                        if let Some((_struct_name, pub_)) = c.for_struct.clone() {
                            if StructInstance::can_access(pub_, public_access) {
                                Ok(c.val.clone())
                            } else {
                                Err(RuntimeError::from_token(
                                    token,
                                    format!(
                                        "Cannot access private static member \"{}\"",
                                        static_name
                                    ),
                                ))
                            }
                        } else {
                            Ok(c.val.clone())
                        }
                    }
                    EnvVal::Function(f) => {
                        if let Some((_struct_name, pub_)) = f.for_struct.clone() {
                            if StructInstance::can_access(pub_, public_access) {
                                Ok(f.val.clone())
                            } else {
                                Err(RuntimeError::from_token(
                                    token,
                                    format!(
                                        "Cannot access private static member \"{}\"",
                                        static_name
                                    ),
                                ))
                            }
                        } else {
                            Ok(f.val.clone())
                        }
                    }
                    _ => Err(RuntimeError::from_token(
                        token,
                        "Must be a static access".to_string(),
                    )),
                }
            }
            Val::Enum(token) => {
                let static_name = construct_static_name(&token.lexeme, &expr.prop_name.lexeme);
                let static_val = self.env.borrow_mut().get_by_str(&static_name, token.pos)?;
                let env_val = static_val.borrow_mut();

                match env_val.deref() {
                    EnvVal::EnumValue(e) => Ok(e.val.clone()),
                    _ => Err(RuntimeError::from_token(
                        token,
                        "Enum value expected".to_string(),
                    )),
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Unknown static callee type \"{}\"",
                static_caller.get_type()
            ))),
        }
    }

    fn eval_get_prop_expr(&mut self, expr: &GetProp) -> Result<Val> {
        let instance = self.evaluate(&expr.name)?;
        match instance {
            Val::StructInstance(i) => {
                let struct_name = i.borrow().struct_name.clone();
                let public_access = self.is_public_access(struct_name);
                let val = i.borrow_mut().get_prop(&expr.prop_name, public_access)?;
                match val {
                    PropFuncVal::Prop(val) => Ok(val),
                    PropFuncVal::Func((func, self_, _pub)) => {
                        Ok(self.eval_fn_expr(&func, Some(self_), None))
                    }
                }
            }
            Val::VecInstance(vec) => VecInstance::get_method(&expr.prop_name, vec),
            _ => Err(RuntimeError::new(format!(
                "Must be a struct instance, got \"{}\"",
                instance.get_type()
            ))),
        }
    }

    fn eval_set_prop_expr(&mut self, expr: &SetProp) -> Result<Val> {
        let instance = self.evaluate(&expr.name)?;
        let instance = if let Val::StructInstance(i) = instance {
            i
        } else {
            return Err(RuntimeError::new("Must be a struct instance".to_string()));
        };

        let val = self.evaluate(&expr.expr)?;
        let val = match expr.operator.token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual => {
                let struct_name = instance.borrow().struct_name.clone();
                let public_access = self.is_public_access(struct_name);

                let r_val = instance
                    .borrow_mut()
                    .get_prop(&expr.prop_name, public_access)?;
                let r_val = match r_val {
                    PropFuncVal::Prop(val) => val,
                    _ => {
                        return Err(RuntimeError::from_token(
                            expr.operator.clone(),
                            "Must be a property".to_string(),
                        ))
                    }
                };

                Self::evaluate_two_operands(expr.operator.clone(), val, r_val)?
            }
            _ => {
                return Err(RuntimeError::from_token(
                    expr.operator.clone(),
                    "Unrecognised token in an assignment expression".to_string(),
                ))
            }
        };

        let struct_name = instance.borrow().struct_name.clone();
        let public_access = self.is_public_access(struct_name);
        let mut instance = instance.borrow_mut();

        instance.set_prop(&expr.prop_name, val.clone(), public_access)?;

        Ok(val)
    }

    fn eval_set_index_expr(&mut self, expr: &SetIndex) -> Result<Val> {
        let vec = self.evaluate(&expr.name)?;
        let vec = if let Val::VecInstance(v) = vec {
            v
        } else if let Val::Uninit = vec {
            return Err(RuntimeError::new("Out of bounds".to_string()));
        } else {
            return Err(RuntimeError::new(format!(
                "Must be a vec instance, got \"{}\"",
                vec
            )));
        };

        let index = self.evaluate(&expr.index)?;
        let index = if let Val::Int(int) = index {
            int as usize
        } else {
            return Err(RuntimeError::from_token(
                expr.operator.clone(),
                format!(
                    "Values of type \"{}\" can have indices of type \"{}\", got \"{}\"",
                    TYPE_VEC,
                    TYPE_INT,
                    index.get_type()
                ),
            ));
        };

        let val = self.evaluate(&expr.expr)?;
        let mut vec = vec.borrow_mut();

        if !vec.val_type.conforms(&val) {
            return Err(RuntimeError::from_token(
                expr.operator.clone(),
                format!(
                    "Cannot assign value of type \"{}\" to a vector of type \"{}\"",
                    val.get_type(),
                    vec.val_type
                ),
            ));
        }

        let val = match expr.operator.token_type {
            TokenType::Equal => val,
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::AsteriskEqual
            | TokenType::SlashEqual
            | TokenType::ModulusEqual => {
                let l_val = vec.get(index)?;
                Self::evaluate_two_operands(expr.operator.clone(), l_val, val)?
            }
            _ => {
                return Err(RuntimeError::from_token(
                    expr.operator.clone(),
                    "Unrecognised token in an assignment expression".to_string(),
                ))
            }
        };

        vec.set(index, val.clone())?;

        Ok(val)
    }

    fn eval_logical_binary_expr(&mut self, expr: &Binary) -> Result<Val> {
        let left = self.evaluate(&expr.left)?;
        if let Val::Bool(l_val) = left {
            if expr.operator.token_type == TokenType::LogicOr {
                if l_val {
                    return Ok(left);
                }
            } else if !l_val {
                return Ok(left);
            }
        } else {
            return Err(RuntimeError::from_token(
                expr.operator.clone(),
                format!(
                    "Only boolean values can be used in logical expressions, got \"{}\"",
                    left.get_type()
                ),
            ));
        }

        let right = self.evaluate(&expr.right)?;
        if let Val::Bool(_) = right {
            Ok(right)
        } else {
            Err(RuntimeError::from_token(
                expr.operator.clone(),
                format!(
                    "Only boolean values can be used in logical expressions, got \"{}\"",
                    right.get_type()
                ),
            ))
        }
    }

    fn eval_binary_expr(&mut self, expr: &Binary) -> Result<Val> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        let val = Self::evaluate_two_operands(expr.operator.clone(), left, right)?;

        Ok(val)
    }

    fn evaluate_two_operands(operator: Token, left: Val, right: Val) -> Result<Val> {
        let val = match operator.token_type {
            //equality
            TokenType::BangEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => {
                    Val::Bool((left - right).abs() > Val::FLOAT_ERROR_MARGIN)
                }
                (Val::Int(left), Val::Int(right)) => Val::Bool(left != right),
                (Val::Int(left), Val::Float(right)) => {
                    Val::Bool(((left as f64) - right).abs() > Val::FLOAT_ERROR_MARGIN)
                }
                (Val::Float(left), Val::Int(right)) => {
                    Val::Bool((left - (right as f64)).abs() > Val::FLOAT_ERROR_MARGIN)
                }

                (Val::Str(left), Val::Str(right)) => Val::Bool(left != right),
                (Val::Bool(left), Val::Bool(right)) => Val::Bool(left != right),
                (Val::Nil, Val::Nil) => Val::Bool(true),
                (Val::EnumValue(e1, _, v1), Val::EnumValue(e2, _, v2)) if e1 == e2 => {
                    Val::Bool(v1 != v2)
                }
                (l, r) => {
                    return Err(equal_types_expected_error(
                        operator,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::EqualEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => {
                    Val::Bool((left - right).abs() < Val::FLOAT_ERROR_MARGIN)
                }
                (Val::Int(left), Val::Int(right)) => Val::Bool(left == right),
                (Val::Float(left), Val::Int(right)) => {
                    Val::Bool((left - (right as f64)).abs() < Val::FLOAT_ERROR_MARGIN)
                }
                (Val::Int(left), Val::Float(right)) => {
                    Val::Bool(((left as f64) - right).abs() < Val::FLOAT_ERROR_MARGIN)
                }
                (Val::Str(left), Val::Str(right)) => Val::Bool(left == right),
                (Val::Bool(left), Val::Bool(right)) => Val::Bool(left == right),
                (Val::Nil, Val::Nil) => Val::Bool(true),
                (Val::EnumValue(e1, _, v1), Val::EnumValue(e2, _, v2)) if e1 == e2 => {
                    Val::Bool(v1 == v2)
                }
                (l, r) => {
                    return Err(equal_types_expected_error(
                        operator,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            // comparison
            TokenType::Greater => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Bool(left > right),
                (Val::Int(left), Val::Int(right)) => Val::Bool(left > right),
                (Val::Float(left), Val::Int(right)) => Val::Bool(left > (right as f64)),
                (Val::Int(left), Val::Float(right)) => Val::Bool((left as f64) > right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_BOOL,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::GreaterEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Bool(left >= right),
                (Val::Int(left), Val::Int(right)) => Val::Bool(left >= right),
                (Val::Float(left), Val::Int(right)) => Val::Bool(left >= right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Bool((left as f64) >= right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_BOOL,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::Less => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Bool(left < right),
                (Val::Int(left), Val::Int(right)) => Val::Bool(left < right),
                (Val::Float(left), Val::Int(right)) => Val::Bool(left < right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Bool((left as f64) < right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_BOOL,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::LessEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Bool(left <= right),
                (Val::Int(left), Val::Int(right)) => Val::Bool(left <= right),
                (Val::Float(left), Val::Int(right)) => Val::Bool(left <= right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Bool((left as f64) <= right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_BOOL,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            // math
            TokenType::Minus | TokenType::MinusEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Float(left - right),
                (Val::Int(left), Val::Int(right)) => Val::Int(left - right),
                (Val::Float(left), Val::Int(right)) => Val::Float(left - right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Float((left as f64) - right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_NUM,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::Plus | TokenType::PlusEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Float(left + right),
                (Val::Int(left), Val::Int(right)) => Val::Int(left + right),
                (Val::Float(left), Val::Int(right)) => Val::Float(left + right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Float((left as f64) + right),

                (Val::Str(left), Val::Str(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Str(left), Val::Float(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Str(left), Val::Int(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Float(left), Val::Str(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Int(left), Val::Str(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Str(left), Val::Bool(right)) => Val::Str(format!("{}{}", left, right)),
                (Val::Bool(left), Val::Str(right)) => Val::Str(format!("{}{}", left, right)),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        &format!("{}, {}", TYPE_NUM, TYPE_STR),
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::Slash | TokenType::SlashEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Float(left / right),
                (Val::Int(left), Val::Int(right)) => Val::Int(left / right),
                (Val::Float(left), Val::Int(right)) => Val::Float(left / right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Float((left as f64) / right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_NUM,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::Modulus | TokenType::ModulusEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Float(left % right),
                (Val::Int(left), Val::Int(right)) => Val::Int(left % right),
                (Val::Float(left), Val::Int(right)) => Val::Float(left % right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Float((left as f64) % right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_NUM,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            TokenType::Asterisk | TokenType::AsteriskEqual => match (left, right) {
                (Val::Float(left), Val::Float(right)) => Val::Float(left * right),
                (Val::Int(left), Val::Int(right)) => Val::Int(left * right),
                (Val::Float(left), Val::Int(right)) => Val::Float(left * right as f64),
                (Val::Int(left), Val::Float(right)) => Val::Float((left as f64) * right),
                (l, r) => {
                    return Err(incompatible_types_error(
                        operator,
                        TYPE_NUM,
                        &r.get_type(),
                        &l.get_type(),
                    ))
                }
            },
            // FIXME: add bitwise operations
            // bitwise
            // TokenType::BitwiseAnd => match (left, right) {
            //     (Val::Number(left), Val::Number(right)) => Val::Number(left & right),
            //     (l, r) => return Err(incompatible_types_error(operator.clone(), ValType::TYPE_NUM, &r.get_type(), &l.get_type())),
            // },
            // TokenType::BitwiseOr => match (left, right) {
            //     (Val::Number(left), Val::Number(right)) => Val::Number(left | right),
            //     (l, r) => return Err(incompatible_types_error(operator.clone(), ValType::TYPE_NUM, &r.get_type(), &l.get_type())),
            // },
            _ => {
                return Err(RuntimeError::from_token(
                    operator.clone(),
                    format!("Unknown binary operator \"{}\"", operator.lexeme),
                ))
            }
        };

        Ok(val)
    }

    fn eval_grouping_expr(&mut self, expr: &Grouping) -> Result<Val> {
        self.evaluate(&expr.expr)
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

    fn evaluate(&mut self, expr: &Expr) -> Result<Val> {
        use Expr::*;
        let val = match expr {
            EmptyExpr => Val::Nil,
            NilLiteralExpr(literal) => self.eval_nil_literal(&literal),
            BoolLiteralExpr(literal) => self.eval_bool_literal(&literal),
            IntLiteralExpr(literal) => self.eval_int_literal(&literal),
            FloatLiteralExpr(literal) => self.eval_float_literal(&literal),
            StrLiteralExpr(literal) => self.eval_str_literal(&literal),
            UnaryExpr(unary) => self.eval_unary_expr(&unary)?,
            CallExpr(call) => self.eval_call_expr(&call)?,
            SelfStaticExpr(self_static) => self.eval_self_static_expr(&self_static)?,
            SelfExpr(self_) => self.eval_self_expr(&self_)?,
            CallStructExpr(call_struct) => self.eval_call_struct_expr(&call_struct)?,
            VecExpr(call_vec) => self.eval_vec_expr(&call_vec)?,
            VecIndexExpr(vec_indx) => self.eval_vec_index(&vec_indx)?,
            GetStaticExpr(get_static_prop) => self.eval_get_static_prop_expr(&get_static_prop)?,
            GetPropExpr(get_prop) => self.eval_get_prop_expr(&get_prop)?,
            SetPropExpr(set_prop) => self.eval_set_prop_expr(&set_prop)?,
            SetIndexExpr(set_index) => self.eval_set_index_expr(&set_index)?,
            BinaryExpr(binary) => self.eval_binary_expr(&binary)?,
            LogicalBinaryExpr(l_binary) => self.eval_logical_binary_expr(&l_binary)?,
            GroupingExpr(grouping) => self.eval_grouping_expr(&grouping)?,
            VariableExpr(variable) => self.eval_var_expr(&variable)?,
            AssignmentExpr(assignment) => self.eval_assign_expr(&assignment)?,
            FnExpr(lambda) => self.eval_fn_expr(&lambda, None, None),
            MatchExpr(match_expr) => self.eval_match_expr(match_expr)?,
        };

        Ok(val)
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<StmtVal> {
        use Stmt::*;
        match stmt {
            Expr(expr_stmt) => self.eval_expr_stmt(expr_stmt),
            Let(var_decl) => self.eval_var_stmt(var_decl),
            Const(const_decl) => self.eval_const_stmt(const_decl),
            Break => Ok(self.eval_break_stmt()),
            Continue => Ok(self.eval_continue_stmt()),
            Return(return_stmt) => self.eval_return_stmt(return_stmt),
            BlockStmt(block) => self.eval_block_stmt(block),
            IfStmt(if_stmt) => self.eval_if_stmt(if_stmt),
            Fn(f_decl) => self.eval_fn_stmt(f_decl),
            LoopStmt(loop_stmt) => self.eval_loop_stmt(loop_stmt),
            Enum(enum_decl) => self.eval_enum_stmt(enum_decl),
            Struct(struct_decl) => self.eval_struct_stmt(struct_decl),
            Impl(impl_decl) => self.eval_impl_stmt(impl_decl),
        }
    }

    pub fn evaluate_block(
        &mut self,
        stmts: &[Stmt],
        env: Option<Rc<RefCell<Env>>>,
    ) -> Result<StmtVal> {
        let new_env = if let Some(env) = env {
            env
        } else {
            Rc::new(RefCell::new(Env::with_enclosing(Rc::clone(&self.env))))
        };

        let old_env = mem::replace(&mut self.env, new_env);

        for stmt in stmts {
            let stmt_val = self.evaluate_stmt(stmt)?;
            match stmt_val {
                StmtVal::None => {}
                StmtVal::Break => {
                    self.env = old_env;

                    return Ok(StmtVal::Break);
                }
                StmtVal::Continue => {
                    self.env = old_env;

                    return Ok(StmtVal::Continue);
                }
                StmtVal::Return(val) => {
                    self.env = old_env;

                    return Ok(StmtVal::Return(val));
                }
            }
        }

        self.env = old_env;

        Ok(StmtVal::None)
    }

    fn is_public_access(&self, struct_name: String) -> bool {
        if let Some(self_) = self.env.borrow().get_self() {
            self_.borrow().struct_name != struct_name
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

    fn is_true(val: &Val) -> Result<bool> {
        match val {
            Val::Bool(true) => Ok(true),
            Val::Bool(false) => Ok(false),
            _ => Err(RuntimeError::new(format!(
                "Trying to evaluate value of type \"{}\" as boolean",
                val.get_type()
            ))),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Option<Token>,
    pub pos: Option<Pos>,
    pub msg: String,
}

impl RuntimeError {
    pub fn from_pos(pos: Pos, msg: String) -> Self {
        Self {
            pos: Some(pos),
            token: None,
            msg,
        }
    }

    pub fn from_token(token: Token, msg: String) -> Self {
        Self {
            pos: Some(token.pos),
            token: Some(token),
            msg,
        }
    }

    pub fn new(msg: String) -> Self {
        Self {
            pos: None,
            token: None,
            msg,
        }
    }
}

fn incompatible_types_error(
    token: Token,
    expected: &str,
    actual_l: &str,
    actual_r: &str,
) -> RuntimeError {
    return RuntimeError::from_token(
        token,
        format!(
            "Both operands must be of type \"{}\". Got \"{}\" and \"{}\"",
            expected, actual_l, actual_r,
        ),
    );
}

fn equal_types_expected_error(token: Token, actual_l: &str, actual_r: &str) -> RuntimeError {
    return RuntimeError::from_token(
        token,
        format!(
            "Both operands must be of the same type. Got \"{}\" and \"{}\"",
            actual_l, actual_r,
        ),
    );
}
