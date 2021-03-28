use crate::interpreter::val::Val;
use crate::lexer::token::TokenType;
use crate::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    EmptyExpr,
    NilLiteralExpr(NilLiteral),
    BoolLiteralExpr(BoolLiteral),
    IntLiteralExpr(IntLiteral),
    FloatLiteralExpr(FloatLiteral),
    StrLiteralExpr(StrLiteral),
    UnaryExpr(Unary),
    SelfExpr(Self_),
    CallExpr(Call),
    CallStructExpr(CallStruct),
    VecExpr(Vec_),
    VecIndexExpr(VecIndex),
    GetPropExpr(GetProp),
    SetPropExpr(SetProp),
    SetIndexExpr(SetIndex),
    FnExpr(Lambda),
    BinaryExpr(Binary),
    LogicalBinaryExpr(Binary),
    GroupingExpr(Grouping),
    VariableExpr(Variable),
    AssignmentExpr(Assignment),
    MatchExpr(Match),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Break,
    Continue,
    Expr(Expr),
    Return(Return),
    Let(VarDecl),
    Const(ConstDecl),
    BlockStmt(Block),
    IfStmt(If),
    Fn(FnDecl),
    Struct(StructDecl),
    LoopStmt(Loop),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    Uninit,
    Num,
    Int,
    Float,
    Bool,
    Nil,
    Str,
    Vec(Generics),
    Map,
    Func,
    Struct(String),
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    types: Vec<ValType>,
}

impl Generics {
    pub fn new(types: Vec<ValType>) -> Self {
        Self { types }
    }
}

pub const TYPE_UNINIT: &str = "uninit";
pub const TYPE_ANY: &str = "any";
pub const TYPE_BOOL: &str = "bool";
pub const TYPE_FUNC: &str = "func";
pub const TYPE_NUM: &str = "num";
pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_STR: &str = "str";
pub const TYPE_NIL: &str = "nil";
pub const TYPE_VEC: &str = "vec";
pub const TYPE_MAP: &str = "map";
pub const TYPE_STRUCT: &str = "struct";
pub const TYPE_STRUCT_INSTANCE: &str = "struct";

impl ValType {
    pub fn try_from_token(token: &Token, generics: Option<Vec<Self>>) -> Option<Self> {
        match token.token_type {
            TokenType::Num => Some(Self::Num),
            TokenType::Int => Some(Self::Int),
            TokenType::Float => Some(Self::Float),
            TokenType::Bool => Some(Self::Bool),
            TokenType::Nil => Some(Self::Nil),
            TokenType::Str => Some(Self::Str),
            TokenType::Vec => {
                let generics = generics.unwrap_or(vec![Self::Any]);
                Some(Self::Vec(Generics::new(generics)))
            }
            TokenType::Map => Some(Self::Map),
            TokenType::Func => Some(Self::Func),
            TokenType::Any => Some(Self::Any),
            TokenType::Identifier => Some(Self::Struct(token.lexeme.clone())),
            _ => None,
        }
    }

    pub fn try_from_val(val: &Val) -> Option<Self> {
        match val {
            Val::Uninit => Some(Self::Uninit),
            Val::Float(_) => Some(Self::Float),
            Val::Int(_) => Some(Self::Int),
            Val::Bool(_) => Some(Self::Bool),
            Val::Nil => Some(Self::Nil),
            Val::Str(_) => Some(Self::Str),
            Val::Callable(_) => Some(Self::Func),
            Val::StructInstance(i) => Some(Self::Struct(i.borrow_mut().struct_name.clone())),
            Val::VecInstance(v) => Some(Self::Vec(Generics::new(vec![v
                .borrow_mut()
                .val_type
                .clone()]))),
            _ => None,
        }
    }

    pub fn conforms(&self, val: &Val) -> bool {
        match (self, val) {
            (_, Val::Uninit) => true,
            (Self::Any, _) => true,
            (Self::Nil, Val::Nil) => true,
            (Self::Bool, Val::Bool(_)) => true,
            (Self::Func, Val::Callable(_)) => true,
            (Self::Num, Val::Float(_)) => true,
            (Self::Num, Val::Int(_)) => true,
            (Self::Int, Val::Int(_)) => true,
            (Self::Float, Val::Float(_)) => true,
            (Self::Float, Val::Int(_)) => true,
            (Self::Str, Val::Str(_)) => true,
            (Self::Struct(s), Val::StructInstance(i)) => i.borrow_mut().struct_name == *s,
            (Self::Vec(g), Val::VecInstance(v)) => {
                let v_g_type = g.types.first().unwrap();
                let vi_g_type = v.borrow_mut().val_type.clone();

                *v_g_type == vi_g_type
            }
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Uninit => TYPE_UNINIT.to_string(),
            Self::Any => TYPE_ANY.to_string(),
            Self::Bool => TYPE_BOOL.to_string(),
            Self::Func => TYPE_FUNC.to_string(),
            Self::Num => TYPE_NUM.to_string(),
            Self::Int => TYPE_INT.to_string(),
            Self::Float => TYPE_FLOAT.to_string(),
            Self::Str => TYPE_STR.to_string(),
            Self::Nil => TYPE_NIL.to_string(),
            Self::Vec(g) => format!("{}<{}>", TYPE_VEC, g.types.first().unwrap().to_string()),
            Self::Map => TYPE_MAP.to_string(),
            Self::Struct(s) => s.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NilLiteral;

#[derive(Debug, Clone)]
pub struct BoolLiteral(pub bool);

#[derive(Debug, Clone)]
pub struct IntLiteral(pub isize);

#[derive(Debug, Clone)]
pub struct FloatLiteral(pub f64);

#[derive(Debug, Clone)]
pub struct StrLiteral(pub String);

#[derive(Debug, Clone)]
pub struct Unary {
    pub expr: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct Self_ {
    pub self_: Token,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallStruct {
    pub callee: Box<Expr>,
    pub args: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Vec_ {
    pub vals: Vec<Expr>,
    pub val_type: Option<ValType>,
}

#[derive(Debug, Clone)]
pub struct VecIndex {
    pub callee: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct GetProp {
    pub name: Box<Expr>,
    pub prop_name: Token,
}

#[derive(Debug, Clone)]
pub struct SetProp {
    pub name: Box<Expr>,
    pub prop_name: Token,
    pub operator: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct SetIndex {
    pub name: Box<Expr>,
    pub index: Box<Expr>,
    pub operator: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<(Token, ValType, bool)>,
    pub ret_type: ValType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Token,
    pub operator: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Token,
    pub init: Box<Option<Expr>>,
    pub v_type: Option<ValType>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: Token,
    pub init: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expr>,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: Token,
    pub lambda: Lambda,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Token,
    pub fns: Vec<FnDecl>,
    pub props: Vec<VarDecl>,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub inc: Box<Expr>,
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub keyword: Token,
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub default: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub expr: Box<Expr>,
    pub body: Box<Expr>,
}

impl Expr {
    pub fn is_empty(&self) -> bool {
        use Expr::*;

        match self {
            EmptyExpr => true,
            _ => false,
        }
    }
}

impl Binary {
    pub fn new(left: Box<Expr>, right: Box<Expr>, operator: Token) -> Self {
        Self {
            left,
            right,
            operator,
        }
    }
}

impl Unary {
    pub fn new(expr: Box<Expr>, operator: Token) -> Self {
        Self { expr, operator }
    }
}

impl Call {
    pub fn new(callee: Box<Expr>, args: Vec<Expr>) -> Self {
        Self { callee, args }
    }
}

impl Self_ {
    pub fn new(self_: Token) -> Self {
        Self { self_ }
    }
}

impl CallStruct {
    pub fn new(callee: Box<Expr>, args: Vec<(Token, Expr)>) -> Self {
        Self { callee, args }
    }
}

impl Vec_ {
    pub fn new(vals: Vec<Expr>, val_type: Option<ValType>) -> Self {
        Self { vals, val_type }
    }
}

impl VecIndex {
    pub fn new(callee: Box<Expr>, index: Box<Expr>) -> Self {
        Self { callee, index }
    }
}

impl GetProp {
    pub fn new(name: Box<Expr>, prop_name: Token) -> Self {
        Self { name, prop_name }
    }
}

impl SetProp {
    pub fn new(name: Box<Expr>, prop_name: Token, operator: Token, expr: Box<Expr>) -> Self {
        Self {
            name,
            prop_name,
            operator,
            expr,
        }
    }
}

impl SetIndex {
    pub fn new(name: Box<Expr>, index: Box<Expr>, operator: Token, expr: Box<Expr>) -> Self {
        Self {
            name,
            index,
            operator,
            expr,
        }
    }
}

impl Grouping {
    pub fn new(expr: Box<Expr>) -> Self {
        Self { expr }
    }
}

impl Variable {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

impl Assignment {
    pub fn new(name: Token, operator: Token, expr: Box<Expr>) -> Self {
        Self {
            name,
            operator,
            expr,
        }
    }
}

impl VarDecl {
    pub fn new(
        name: Token,
        init: Box<Option<Expr>>,
        mutable: bool,
        v_type: Option<ValType>,
    ) -> Self {
        Self {
            name,
            init,
            mutable,
            v_type,
        }
    }
}

impl ConstDecl {
    pub fn new(name: Token, init: Box<Expr>) -> Self {
        Self { name, init }
    }
}

impl Block {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}

impl If {
    pub fn new(condition: Box<Expr>, then_stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>>) -> Self {
        Self {
            condition,
            then_stmt,
            else_stmt,
        }
    }
}

impl Return {
    pub fn new(keyword: Token, expr: Box<Expr>) -> Self {
        Self { keyword, expr }
    }
}

impl FnDecl {
    pub const MAX_ARGS: usize = 127;

    pub fn new(name: Token, lambda: Lambda) -> Self {
        Self { name, lambda }
    }
}

impl StructDecl {
    pub fn new(name: Token, props: Vec<VarDecl>, fns: Vec<FnDecl>) -> Self {
        Self { name, fns, props }
    }
}

impl Lambda {
    #[allow(dead_code)]
    pub const MAX_ARGS: usize = FnDecl::MAX_ARGS;

    pub fn new(params: Vec<(Token, ValType, bool)>, ret_type: ValType, body: Vec<Stmt>) -> Self {
        Self {
            params,
            ret_type,
            body,
        }
    }
}

impl Loop {
    pub fn new(inc: Box<Expr>, condition: Box<Expr>, body: Box<Stmt>) -> Self {
        Self {
            inc,
            condition,
            body,
        }
    }
}

impl Match {
    pub fn new(
        keyword: Token,
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        default: Option<Box<Expr>>,
    ) -> Self {
        Self {
            keyword,
            expr,
            arms,
            default,
        }
    }
}

impl MatchArm {
    pub fn new(expr: Box<Expr>, body: Box<Expr>) -> Self {
        Self { expr, body }
    }
}
