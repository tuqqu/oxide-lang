use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use oxide_parser::expr::Lambda;
use oxide_parser::stmt::StructDecl;
use oxide_parser::valtype::{
    FnType, Generics, ValType, TYPE_ANY, TYPE_BOOL, TYPE_ENUM, TYPE_FLOAT, TYPE_INT, TYPE_NIL,
    TYPE_STR, TYPE_STRUCT, TYPE_TRAIT, TYPE_TYPE, TYPE_UNINIT, TYPE_VEC,
};
use oxide_parser::Token;

use crate::env::Env;
use crate::env_val::{construct_static_name, Impl};
use crate::error::RuntimeError;
use crate::interpreter::{InterpretedResult, Interpreter};

static COUNTER: AtomicUsize = AtomicUsize::new(1);

fn internal_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Clone)]
pub enum Val {
    Uninit,
    Nil,
    Bool(bool),
    Str(String),
    Int(isize),
    Float(f64),
    Callable(Callable),
    Struct(Token, StructCallable),
    StructInstance(Rc<RefCell<StructInstance>>),
    Enum(Token),
    Type(ValType),
    EnumValue(String, String, usize),
    VecInstance(Rc<RefCell<VecInstance>>),
    Trait(Token),
    Any(Box<Self>),
}

impl Val {
    const FLOAT_ERROR_MARGIN: f64 = f64::EPSILON;

    pub(crate) fn new_vec_instance(vals: &[Self], vtype: ValType) -> Self {
        Self::VecInstance(Rc::new(RefCell::new(self::VecInstance::new(
            vals.to_vec(),
            vtype,
        ))))
    }

    pub(crate) fn equal(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Nil, Nil) => true,
            (Nil, _) => false,
            (_, Nil) => false,
            (Bool(lhs), Bool(rhs)) => lhs == rhs,
            (Str(lhs), Str(rhs)) => lhs == rhs,
            (Int(lhs), Int(rhs)) => lhs == rhs,
            (Float(lhs), Float(rhs)) => (lhs - rhs).abs() < Self::FLOAT_ERROR_MARGIN,
            (Float(lhs), Int(rhs)) => (lhs - (*rhs as f64)).abs() < Self::FLOAT_ERROR_MARGIN,
            (Int(lhs), Float(rhs)) => ((*lhs as f64) - rhs).abs() < Self::FLOAT_ERROR_MARGIN,
            (EnumValue(lhs_e, _, lhs_v), EnumValue(rhs_e, _, rhs_v)) if lhs_e == rhs_e => {
                lhs_v == rhs_v
            }
            (VecInstance(lhs), VecInstance(rhs)) => {
                lhs.borrow().deref().id == rhs.borrow().deref().id
            }
            (StructInstance(lhs), StructInstance(rhs)) => {
                lhs.borrow().deref().id == rhs.borrow().deref().id
            }
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of the same type. Got \"{}\" and \"{}\"",
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn not_equal(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Nil, Nil) => false,
            (Nil, _) => true,
            (_, Nil) => true,
            (Bool(lhs), Bool(rhs)) => lhs != rhs,
            (Str(lhs), Str(rhs)) => lhs != rhs,
            (Int(lhs), Int(rhs)) => lhs != rhs,
            (Float(lhs), Float(rhs)) => (lhs - rhs).abs() > Self::FLOAT_ERROR_MARGIN,
            (Float(lhs), Int(rhs)) => (lhs - (*rhs as f64)).abs() > Self::FLOAT_ERROR_MARGIN,
            (Int(lhs), Float(rhs)) => ((*lhs as f64) - rhs).abs() > Self::FLOAT_ERROR_MARGIN,
            (EnumValue(lhs_e, _, lhs_v), EnumValue(rhs_e, _, rhs_v)) if lhs_e == rhs_e => {
                lhs_v != rhs_v
            }
            (VecInstance(lhs), VecInstance(rhs)) => {
                lhs.borrow().deref().id != rhs.borrow().deref().id
            }
            (StructInstance(lhs), StructInstance(rhs)) => {
                lhs.borrow().deref().id != rhs.borrow().deref().id
            }
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of the same type. Got \"{}\" and \"{}\"",
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn greater(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => lhs > rhs,
            (Float(lhs), Float(rhs)) => lhs > rhs,
            (Float(lhs), Int(rhs)) => *lhs > (*rhs as f64),
            (Int(lhs), Float(rhs)) => (*lhs as f64) > *rhs,
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn greater_equal(
        lhs: &Self,
        rhs: &Self,
        operator: &Token,
    ) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => lhs >= rhs,
            (Float(lhs), Float(rhs)) => lhs >= rhs,
            (Float(lhs), Int(rhs)) => *lhs >= (*rhs as f64),
            (Int(lhs), Float(rhs)) => (*lhs as f64) >= *rhs,
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn less(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => lhs < rhs,
            (Float(lhs), Float(rhs)) => lhs < rhs,
            (Float(lhs), Int(rhs)) => *lhs < (*rhs as f64),
            (Int(lhs), Float(rhs)) => (*lhs as f64) < *rhs,
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn less_equal(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => lhs <= rhs,
            (Float(lhs), Float(rhs)) => lhs <= rhs,
            (Float(lhs), Int(rhs)) => *lhs <= (*rhs as f64),
            (Int(lhs), Float(rhs)) => (*lhs as f64) <= *rhs,
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(Self::Bool(val))
    }

    pub(crate) fn subtract(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs - rhs),
            (Float(lhs), Float(rhs)) => Float(lhs - rhs),
            (Float(lhs), Int(rhs)) => Float(lhs - (*rhs as f64)),
            (Int(lhs), Float(rhs)) => Float((*lhs as f64) - *rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn add(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs + rhs),
            (Float(lhs), Float(rhs)) => Float(lhs + rhs),
            (Float(lhs), Int(rhs)) => Float(lhs + (*rhs as f64)),
            (Int(lhs), Float(rhs)) => Float((*lhs as f64) + *rhs),
            (Str(lhs), Str(rhs)) => Str(format!("{}{}", lhs, rhs)),
            (lhs, rhs) => return Err(
                RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\", \"{}\", or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        TYPE_STR,
                        lhs.get_type(),
                        rhs.get_type(),
                    )
                )
            ),
        };

        Ok(val)
    }

    pub(crate) fn divide(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs / rhs),
            (Float(lhs), Float(rhs)) => Float(lhs / rhs),
            (Float(lhs), Int(rhs)) => Float(lhs / (*rhs as f64)),
            (Int(lhs), Float(rhs)) => Float((*lhs as f64) / rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn modulus(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs % rhs),
            (Float(lhs), Float(rhs)) => Float(lhs % rhs),
            (Float(lhs), Int(rhs)) => Float(lhs % (*rhs as f64)),
            (Int(lhs), Float(rhs)) => Float((*lhs as f64) % *rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn multiply(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs * rhs),
            (Float(lhs), Float(rhs)) => Float(lhs * rhs),
            (Float(lhs), Int(rhs)) => Float(lhs * (*rhs as f64)),
            (Int(lhs), Float(rhs)) => Float((*lhs as f64) * rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of types \"{}\" or \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        TYPE_FLOAT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn bitwise_and(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs & rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of type \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn bitwise_or(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs | rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of type \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn bitwise_xor(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => Int(lhs ^ rhs),
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Both operands must be of type \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn range(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => {
                let vec: Vec<Val> = (*lhs..*rhs).map(Val::Int).collect();
                Self::VecInstance(Rc::new(RefCell::new(self::VecInstance::new(
                    vec,
                    ValType::Int,
                ))))
            }
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Range boundaries must be of type \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn range_equal(lhs: &Self, rhs: &Self, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (lhs, rhs) {
            (Int(lhs), Int(rhs)) => {
                let vec: Vec<Val> = (*lhs..=*rhs).map(Val::Int).collect();
                Self::VecInstance(Rc::new(RefCell::new(self::VecInstance::new(
                    vec,
                    ValType::Int,
                ))))
            }
            (lhs, rhs) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Range boundaries must be of type \"{}\". Got \"{}\" and \"{}\"",
                        TYPE_INT,
                        lhs.get_type(),
                        rhs.get_type(),
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn cast_to(&self, to_type: &ValType, operator: &Token) -> InterpretedResult<Self> {
        use Val::*;
        let val = match (&self, to_type) {
            (Nil, ValType::Int) => Int(0),
            (Nil, ValType::Float) => Float(0_f64),
            (Nil, ValType::Bool) => Bool(false),
            (Nil, ValType::Nil) => Nil,
            (Nil, ValType::Str) => Str(str::to_string("")),

            (Bool(val), ValType::Int) => Int(if *val { 0 } else { 1 }),
            (Bool(val), ValType::Float) => Float(if *val { 0_f64 } else { 1_f64 }),
            (Bool(val), ValType::Bool) => Bool(*val),
            (Bool(_val), ValType::Nil) => Nil,
            (Bool(val), ValType::Str) => Str(val.to_string()),

            (Int(val), ValType::Int) => Int(*val),
            (Int(val), ValType::Float) => Float(*val as f64),
            (Int(val), ValType::Bool) => Bool(*val != 0),
            (Int(_val), ValType::Nil) => Nil,
            (Int(val), ValType::Str) => Str(val.to_string()),

            (Float(val), ValType::Int) => Int(*val as isize),
            (Float(val), ValType::Float) => Float(*val),
            (Float(val), ValType::Bool) => Bool(*val != 0_f64),
            (Float(_val), ValType::Nil) => Nil,
            (Float(val), ValType::Str) => Str(val.to_string()),

            (Str(val), ValType::Int) => Int(val.parse::<isize>().unwrap_or(0)),
            (Str(val), ValType::Float) => Float(val.parse::<f64>().unwrap_or(0_f64)),
            (Str(val), ValType::Bool) => Bool(!val.eq("")),
            (Str(_val), ValType::Nil) => Nil,
            (Str(val), ValType::Str) => Str(val.clone()),

            (Any(val), vt) => val.cast_to(vt, operator)?,

            (_, ValType::Any)
            | (_, ValType::Instance(_))
            | (_, ValType::Vec(_))
            | (_, ValType::Fn(_))
            | (_, ValType::Num)
            | (_, ValType::Map) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!("Value cannot be cast to type \"{}\"", to_type,),
                ))
            }
            (val, to_type) => {
                return Err(RuntimeError::Type(
                    Some(operator.clone()),
                    format!(
                        "Value of type \"{}\" cannot be cast to type \"{}\".",
                        val.get_type(),
                        to_type,
                    ),
                ))
            }
        };

        Ok(val)
    }

    pub(crate) fn get_type(&self) -> String {
        use Val::*;

        match self {
            Uninit => TYPE_UNINIT.to_string(),
            Nil => TYPE_NIL.to_string(),
            Bool(_bool) => TYPE_BOOL.to_string(),
            Str(_str) => TYPE_STR.to_string(),
            Int(_isize) => TYPE_INT.to_string(),
            Float(_f64) => TYPE_FLOAT.to_string(),
            Callable(f) => FnType::construct_type(&f.param_types, &f.ret_type),
            Struct(_t, _c) => TYPE_STRUCT.to_string(),
            StructInstance(i) => i.borrow().struct_name.clone(),
            Enum(_e) => TYPE_ENUM.to_string(),
            EnumValue(e, _n, _v) => e.clone(),
            VecInstance(v) => format!("{}<{}>", TYPE_VEC, v.borrow_mut().val_type),
            Trait(_t) => TYPE_TRAIT.to_string(),
            Any(_v) => TYPE_ANY.to_string(),
            Type(_v) => TYPE_TYPE.to_string(),
        }
    }

    pub(crate) fn as_string(&self) -> InterpretedResult<String> {
        match self {
            Val::Str(s) => Ok(s.clone()),
            _ => Err(RuntimeError::Type(
                None,
                format!(
                    "Value of type \"{}\" cannot be used as \"{}\".",
                    self.get_type(),
                    TYPE_STR,
                ),
            )),
        }
    }

    pub(crate) fn debug(&self) -> String {
        use Val::*;
        match self {
            Uninit => String::from(TYPE_UNINIT),
            Nil => String::from(TYPE_NIL),
            Bool(b) => String::from(if *b { "true" } else { "false" }),
            Str(s) => s.clone(),
            Int(n) => n.to_string(),
            Float(n) => n.to_string(),
            Callable(c) => format!(
                "[fn] {}",
                FnType::construct_type(&c.param_types, &c.ret_type)
            ),
            Struct(t, _c) => format!("[struct {}]", t.lexeme),
            StructInstance(i) => {
                let mut props = vec![];
                for (prop, (val, _val_t, _pub)) in &i.borrow_mut().props {
                    props.push(format!("{}: {}", prop, val));
                }

                format!(
                    "[struct] {} {{ {} }}",
                    i.borrow_mut().struct_name,
                    props.join(", ")
                )
            }
            Enum(e) => format!("[enum {}]", e.lexeme),
            EnumValue(s, n, _v) => format!("[enum] {}", construct_static_name(s, n)),
            VecInstance(v) => {
                let mut vals = vec![];
                for val in &v.borrow_mut().vals {
                    vals.push(val.to_string());
                }

                format!("[vec] [{}]", vals.join(", "))
            }
            Trait(t) => format!("[trait {}]", t.lexeme),
            Any(v) => format!("[any] {}", v),
            Type(v) => format!("[type] {}", v),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug())
    }
}

#[derive(Debug)]
pub(crate) enum StmtVal {
    None,
    Break,
    Continue,
    Return(Val),
}

type OxideCallable<V> = dyn Fn(&mut Interpreter, &[V]) -> InterpretedResult<Val>;

pub(crate) type OxideFunction = OxideCallable<Val>;
type OxideConstructor = OxideCallable<(Token, Val)>;

type OxideFnPointer = Arc<OxideFunction>;
type OxideConstructorPointer = Arc<OxideConstructor>;

pub struct Callable {
    arity: usize,
    param_types: Vec<ValType>,
    ret_type: ValType,
    call: Box<OxideFnPointer>,
}

impl Callable {
    pub(crate) fn new_boxed(
        param_types: Vec<ValType>,
        ret_type: ValType,
        call: OxideFnPointer,
    ) -> Box<Self> {
        Box::new(Self {
            arity: param_types.len(),
            param_types,
            ret_type,
            call: Box::new(call),
        })
    }

    pub(crate) fn arity(&self) -> usize {
        self.arity
    }

    pub(crate) fn call(&self) -> &OxideFnPointer {
        &self.call
    }
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            call: self.call.clone(),
            param_types: self.param_types.clone(),
            ret_type: self.ret_type.clone(),
        }
    }
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Callable")
            .field("arity", &self.arity)
            .finish()
    }
}

pub struct StructCallable {
    arity: usize,
    call: Box<OxideConstructorPointer>,
}

impl StructCallable {
    pub(crate) fn new_boxed(arity: usize, call: OxideConstructorPointer) -> Box<Self> {
        Box::new(Self {
            arity,
            call: Box::new(call),
        })
    }

    pub(crate) fn arity(&self) -> usize {
        self.arity
    }

    pub(crate) fn call(&self) -> &OxideConstructorPointer {
        &self.call
    }
}

impl fmt::Debug for StructCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StructCallable")
            .field("arity", &self.arity)
            .finish()
    }
}

impl Clone for StructCallable {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            call: self.call.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructInstance {
    id: usize,
    props: HashMap<String, (Val, ValType, bool)>,
    fns: HashMap<String, (Lambda, Rc<RefCell<Self>>, bool)>,
    struct_name: String,
    impls: Vec<String>,
}

impl StructInstance {
    pub(crate) fn new(struct_: StructDecl, impls: Vec<Impl>) -> Rc<RefCell<Self>> {
        let mut props: HashMap<String, (Val, ValType, bool)> = HashMap::new();
        for (prop, public) in struct_.props() {
            // we can be sure that v_type is always present
            props.insert(
                prop.name().lexeme.to_string(),
                (Val::Uninit, prop.v_type().clone().unwrap(), *public),
            );
        }

        let mut impl_names = vec![];
        for impl_ in &impls {
            if let Some(trait_name) = impl_.trait_name() {
                impl_names.push(trait_name.clone());
            }
        }

        let instance = Self {
            id: internal_id(),
            props,
            fns: HashMap::new(),
            struct_name: struct_.name().lexeme.clone(),
            impls: impl_names,
        };

        let self_ = Rc::new(RefCell::new(instance));
        for impl_ in impls {
            let mut borrowed_self = self_.borrow_mut();
            for (fun, pub_) in impl_.methods() {
                borrowed_self.fns.insert(
                    fun.name().lexeme.to_string(),
                    (fun.lambda().clone(), self_.clone(), *pub_),
                );
            }
        }

        self_
    }

    pub(crate) fn get_prop(
        &self,
        name: &Token,
        public_access: bool,
    ) -> InterpretedResult<PropFuncVal> {
        if !self.props.contains_key(&name.lexeme) {
            if !self.fns.contains_key(&name.lexeme) {
                Err(RuntimeError::Definition(
                    Some(name.clone()),
                    format!("No struct property with name \"{}\"", name.lexeme),
                ))
            } else {
                let func = self.fns.get(&name.lexeme).unwrap();
                if Self::can_access(func.2, public_access) {
                    Ok(PropFuncVal::Func(func.clone()))
                } else {
                    Err(RuntimeError::Definition(
                        Some(name.clone()),
                        format!("Cannot access private method \"{}\"", name.lexeme),
                    ))
                }
            }
        } else {
            match self.props.get(&name.lexeme).unwrap() {
                (Val::Uninit, _, pub_) => {
                    let msg = if Self::can_access(*pub_, public_access) {
                        format!("Cannot access private property \"{}\"", name.lexeme)
                    } else {
                        format!("Property \"{}\" has not yet been initialized.", name.lexeme)
                    };

                    Err(RuntimeError::Definition(Some(name.clone()), msg))
                }
                (val, _, pub_) => {
                    if Self::can_access(*pub_, public_access) {
                        Ok(PropFuncVal::Prop(val.clone()))
                    } else {
                        Err(RuntimeError::Definition(
                            Some(name.clone()),
                            format!("Cannot access private property \"{}\"", name.lexeme),
                        ))
                    }
                }
            }
        }
    }

    pub(crate) fn set_prop(
        &mut self,
        name: &Token,
        val: Val,
        public_access: bool,
    ) -> InterpretedResult<()> {
        if !self.props.contains_key(&name.lexeme) {
            Err(RuntimeError::Definition(
                Some(name.clone()),
                format!("No struct property with name \"{}\"", name.lexeme),
            ))
        } else {
            let (_, v_type, public) = self.props.get(&name.lexeme).unwrap();

            if !*public && public_access {
                return Err(RuntimeError::Definition(
                    Some(name.clone()),
                    format!("Cannot access private property \"{}\"", name.lexeme),
                ));
            }

            let public = *public;
            let v_type = v_type.clone();
            if vtype_conforms_val(&v_type, &val) {
                self.props
                    .insert(name.lexeme.to_string(), (val, v_type, public));

                Ok(())
            } else {
                Err(RuntimeError::Type(
                    Some(name.clone()),
                    format!(
                        "Trying to assign to a variable of type \"{}\" value of type \"{}\"",
                        v_type,
                        val.get_type()
                    ),
                ))
            }
        }
    }

    pub(crate) fn can_access(prop_pub: bool, access_pub: bool) -> bool {
        if prop_pub {
            true
        } else {
            !access_pub
        }
    }

    pub(crate) fn props(&self) -> &HashMap<String, (Val, ValType, bool)> {
        &self.props
    }

    pub(crate) fn props_mut(&mut self) -> &mut HashMap<String, (Val, ValType, bool)> {
        &mut self.props
    }

    pub(crate) fn struct_name(&self) -> &str {
        &self.struct_name
    }
}

#[derive(Debug, Clone)]
pub struct VecInstance {
    id: usize,
    vals: Vec<Val>,
    val_type: ValType,
}

impl VecInstance {
    const POP: &'static str = "pop";
    const PUSH: &'static str = "push";
    const LEN: &'static str = "len";

    pub(crate) fn new(vals: Vec<Val>, val_type: ValType) -> Self {
        Self {
            id: internal_id(),
            vals,
            val_type,
        }
    }

    pub(crate) fn get(&self, i: usize) -> InterpretedResult<Val> {
        let val = if let Some(val) = self.vals.get(i) {
            val.clone()
        } else {
            Val::Uninit
        };

        Ok(val)
    }

    pub(crate) fn set(&mut self, i: usize, val: Val) -> InterpretedResult<()> {
        self.vals[i] = val;

        Ok(())
    }

    pub(crate) fn get_method(name: &Token, vec: Rc<RefCell<Self>>) -> InterpretedResult<Val> {
        let val_type = vec.borrow().val_type.clone();

        let callable = match name.lexeme.as_str() {
            Self::POP => Val::Callable(*Callable::new_boxed(
                vec![],
                val_type,
                Arc::new(move |_inter, _args| {
                    let popped = vec.borrow_mut().vals.pop().unwrap_or(Val::Uninit);

                    Ok(popped)
                }),
            )),
            Self::PUSH => Val::Callable(*Callable::new_boxed(
                vec![val_type],
                ValType::Nil,
                Arc::new(move |_inter, args| {
                    for arg in args {
                        if !vtype_conforms_val(&vec.borrow_mut().val_type, arg) {
                            return Err(RuntimeError::Type(
                                None,
                                format!(
                                    "Cannot push value of type \"{}\" to a vector of type \"{}\"",
                                    try_vtype_from_val(arg).unwrap(),
                                    vec.borrow_mut().val_type
                                ),
                            ));
                        }
                        vec.borrow_mut().vals.push(arg.clone());
                    }

                    Ok(Val::VecInstance(vec.clone()))
                }),
            )),
            Self::LEN => Val::Callable(*Callable::new_boxed(
                vec![],
                ValType::Int,
                Arc::new(move |_inter, _args| Ok(Val::Int(vec.borrow_mut().len() as isize))),
            )),
            _ => {
                return Err(RuntimeError::Runtime(
                    name.clone(),
                    format!("Unknown vec method \"{}\"", name.lexeme),
                ))
            }
        };

        Ok(callable)
    }

    pub(crate) fn vals(&self) -> &[Val] {
        &self.vals
    }

    pub(crate) fn val_type(&self) -> &ValType {
        &self.val_type
    }

    fn len(&self) -> usize {
        self.vals.len()
    }
}

#[derive(Clone)]
pub(crate) struct Function {
    lambda: Lambda,
    env: Rc<RefCell<Env>>,
}

impl Function {
    pub(crate) fn new(lambda: Lambda, env: Rc<RefCell<Env>>) -> Self {
        Self { lambda, env }
    }

    pub(crate) fn lambda(&self) -> &Lambda {
        &self.lambda
    }

    pub(crate) fn env(&self) -> &Rc<RefCell<Env>> {
        &self.env
    }
}

pub(crate) enum PropFuncVal {
    Prop(Val),
    Func((Lambda, Rc<RefCell<StructInstance>>, bool)),
}

pub(crate) fn try_vtype_from_val(val: &Val) -> Option<ValType> {
    Some(match val {
        Val::Uninit => ValType::Uninit,
        Val::Float(_) => ValType::Float,
        Val::Int(_) => ValType::Int,
        Val::Bool(_) => ValType::Bool,
        Val::Nil => ValType::Nil,
        Val::Str(_) => ValType::Str,
        Val::StructInstance(i) => ValType::Instance(i.borrow_mut().struct_name.clone()),
        Val::EnumValue(e, ..) => ValType::Instance(e.clone()),
        Val::VecInstance(v) => ValType::Vec(Generics::new(vec![v.borrow_mut().val_type.clone()])),
        Val::Callable(c) => ValType::Fn(FnType::new(
            c.param_types.clone(),
            Box::new(c.ret_type.clone()),
        )),
        Val::Any(_) => ValType::Any,
        _ => return None,
    })
}

pub(crate) fn vtype_conforms_val(vtype: &ValType, val: &Val) -> bool {
    match (vtype, val) {
        (_, Val::Uninit) => true,
        (ValType::Any, _) => true,
        (ValType::Nil, Val::Nil) => true,
        (ValType::Bool, Val::Bool(_)) => true,
        (ValType::Fn(fn_type), Val::Callable(call)) => {
            *fn_type.ret_type() == call.ret_type && *fn_type.param_types() == call.param_types
        }
        (ValType::Num, Val::Float(_)) => true,
        (ValType::Num, Val::Int(_)) => true,
        (ValType::Int, Val::Int(_)) => true,
        (ValType::Float, Val::Float(_)) => true,
        (ValType::Float, Val::Int(_)) => true,
        (ValType::Str, Val::Str(_)) => true,
        (ValType::Instance(s), Val::StructInstance(i)) => {
            let i = i.borrow();
            i.struct_name == *s || i.impls.contains(s)
        }
        (ValType::Instance(s), Val::EnumValue(e, ..)) => s == e,
        (ValType::Vec(g), Val::VecInstance(v)) => {
            let v_g_type = g.types().first().unwrap();
            let vi_g_type = v.borrow_mut().val_type.clone();

            *v_g_type == vi_g_type
        }
        (ValType::Union(v), val) => {
            for v_type in v {
                if vtype_conforms_val(v_type, val) {
                    return true;
                }
            }

            false
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_try_from_val() {
        let int = Val::Int(100);
        let float = Val::Float(10.1);
        let string = Val::Str(str::to_string("string"));
        let nil = Val::Nil;
        let boolean = Val::Bool(true);

        assert_eq!(try_vtype_from_val(&int).unwrap(), ValType::Int);
        assert_eq!(try_vtype_from_val(&float).unwrap(), ValType::Float);
        assert_eq!(try_vtype_from_val(&string).unwrap(), ValType::Str);
        assert_eq!(try_vtype_from_val(&nil).unwrap(), ValType::Nil);
        assert_eq!(try_vtype_from_val(&boolean).unwrap(), ValType::Bool);
    }

    #[test]
    fn test_conforms() {
        let int = Val::Int(100);
        let float = Val::Float(10.1);
        let string = Val::Str(str::to_string("string"));
        let nil = Val::Nil;
        let boolean = Val::Bool(true);

        assert!(vtype_conforms_val(&ValType::Int, &int));
        assert!(!vtype_conforms_val(&ValType::Int, &float));
        assert!(!vtype_conforms_val(&ValType::Int, &string));

        assert!(vtype_conforms_val(&ValType::Float, &float));
        assert!(vtype_conforms_val(&ValType::Float, &int));
        assert!(!vtype_conforms_val(&ValType::Float, &string));

        assert!(vtype_conforms_val(&ValType::Str, &string));
        assert!(!vtype_conforms_val(&ValType::Str, &int));
        assert!(!vtype_conforms_val(&ValType::Str, &boolean));

        assert!(vtype_conforms_val(&ValType::Any, &string));
        assert!(vtype_conforms_val(&ValType::Any, &nil));
        assert!(vtype_conforms_val(&ValType::Any, &boolean));
        assert!(vtype_conforms_val(&ValType::Any, &int));
        assert!(vtype_conforms_val(&ValType::Any, &float));
    }
}
