use crate::stmt::Block;
use crate::valtype::ValType;
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
    SelfStaticExpr(SelfStatic),
    SelfExpr(Self_),
    CallExpr(Call),
    CallStructExpr(CallStruct),
    VecExpr(Vec_),
    VecIndexExpr(VecIndex),
    GetStaticExpr(GetStaticProp),
    GetPropExpr(GetProp),
    SetPropExpr(SetProp),
    SetIndexExpr(SetIndex),
    FnExpr(Lambda),
    BinaryExpr(Binary),
    LogicalBinaryExpr(Binary),
    TypeCastExpr(TypeCast),
    GroupingExpr(Grouping),
    VariableExpr(Variable),
    AssignmentExpr(Assignment),
    MatchExpr(Match),
}

impl Expr {
    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, Expr::EmptyExpr)
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
    expr: Box<Expr>,
    operator: Token,
}

impl Unary {
    pub(crate) fn new(expr: Expr, operator: Token) -> Self {
        Self {
            expr: Box::new(expr),
            operator,
        }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }
}

#[derive(Debug, Clone)]
pub struct SelfStatic {
    self_static: Token,
}

impl SelfStatic {
    pub(crate) fn new(self_static: Token) -> Self {
        Self { self_static }
    }

    pub fn self_static(&self) -> &Token {
        &self.self_static
    }
}

#[derive(Debug, Clone)]
pub struct Self_ {
    self_: Token,
}

impl Self_ {
    pub(crate) fn new(self_: Token) -> Self {
        Self { self_ }
    }

    pub fn self_(&self) -> &Token {
        &self.self_
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
}

impl Call {
    pub(crate) fn new(callee: Expr, args: Vec<Expr>) -> Self {
        Self {
            callee: Box::new(callee),
            args,
        }
    }

    pub fn callee(&self) -> &Expr {
        &self.callee
    }

    pub fn args(&self) -> &[Expr] {
        &self.args
    }
}

#[derive(Debug, Clone)]
pub struct CallStruct {
    callee: Box<Expr>,
    args: Vec<(Token, Expr)>,
}

impl CallStruct {
    pub(crate) fn new(callee: Expr, args: Vec<(Token, Expr)>) -> Self {
        Self {
            callee: Box::new(callee),
            args,
        }
    }

    pub fn callee(&self) -> &Expr {
        &self.callee
    }

    pub fn args(&self) -> &[(Token, Expr)] {
        &self.args
    }
}

#[derive(Debug, Clone)]
pub struct Vec_ {
    token: Token,
    vals: Vec<Expr>,
    val_type: Option<ValType>,
}

impl Vec_ {
    pub(crate) fn new(vals: Vec<Expr>, val_type: Option<ValType>, token: Token) -> Self {
        Self {
            token,
            vals,
            val_type,
        }
    }

    pub fn token(&self) -> &Token {
        &self.token
    }
    pub fn vals(&self) -> &[Expr] {
        &self.vals
    }

    pub fn val_type(&self) -> &Option<ValType> {
        &self.val_type
    }
}

#[derive(Debug, Clone)]
pub struct VecIndex {
    callee: Box<Expr>,
    index: Box<Expr>,
}

impl VecIndex {
    pub(crate) fn new(callee: Expr, index: Expr) -> Self {
        Self {
            callee: Box::new(callee),
            index: Box::new(index),
        }
    }

    pub fn callee(&self) -> &Expr {
        &self.callee
    }

    pub fn index(&self) -> &Expr {
        &self.index
    }
}

#[derive(Debug, Clone)]
pub struct GetStaticProp {
    name: Box<Expr>,
    prop_name: Token,
}

impl GetStaticProp {
    pub(crate) fn new(name: Expr, prop_name: Token) -> Self {
        Self {
            name: Box::new(name),
            prop_name,
        }
    }

    pub fn name(&self) -> &Expr {
        &self.name
    }

    pub fn prop_name(&self) -> &Token {
        &self.prop_name
    }
}

#[derive(Debug, Clone)]
pub struct GetProp {
    name: Box<Expr>,
    prop_name: Token,
}

impl GetProp {
    pub(crate) fn new(name: Expr, prop_name: Token) -> Self {
        Self {
            name: Box::new(name),
            prop_name,
        }
    }

    pub fn name(&self) -> &Expr {
        &self.name
    }

    pub fn prop_name(&self) -> &Token {
        &self.prop_name
    }
}

#[derive(Debug, Clone)]
pub struct SetProp {
    name: Box<Expr>,
    prop_name: Token,
    operator: Token,
    expr: Box<Expr>,
}

impl SetProp {
    pub(crate) fn new(name: Expr, prop_name: Token, operator: Token, expr: Expr) -> Self {
        Self {
            name: Box::new(name),
            prop_name,
            operator,
            expr: Box::new(expr),
        }
    }

    pub fn name(&self) -> &Expr {
        &self.name
    }

    pub fn prop_name(&self) -> &Token {
        &self.prop_name
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct SetIndex {
    name: Box<Expr>,
    index: Box<Expr>,
    operator: Token,
    expr: Box<Expr>,
}

impl SetIndex {
    pub(crate) fn new(name: Expr, index: Expr, operator: Token, expr: Expr) -> Self {
        Self {
            name: Box::new(name),
            index: Box::new(index),
            operator,
            expr: Box::new(expr),
        }
    }

    pub fn name(&self) -> &Expr {
        &self.name
    }

    pub fn index(&self) -> &Expr {
        &self.index
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

pub type Param = (Token, ValType, bool);

#[derive(Debug, Clone)]
pub struct Lambda {
    params: Vec<Param>,
    ret_type: ValType,
    /// Body of the lambda.
    /// We store Block directly instead of the generic Stmt
    /// Because we need to evaluate it with a custom env
    body: Block,
}

impl Lambda {
    pub(crate) fn new(params: Vec<Param>, ret_type: ValType, body: Block) -> Self {
        Self {
            params,
            ret_type,
            body,
        }
    }

    pub fn params(&self) -> &[Param] {
        &self.params
    }

    pub fn ret_type(&self) -> &ValType {
        &self.ret_type
    }

    pub fn body(&self) -> &Block {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    left: Box<Expr>,
    right: Box<Expr>,
    operator: Token,
}

impl Binary {
    pub(crate) fn new(left: Expr, right: Expr, operator: Token) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }
    }

    pub fn left(&self) -> &Expr {
        &self.left
    }

    pub fn right(&self) -> &Expr {
        &self.right
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }
}

#[derive(Debug, Clone)]
pub struct TypeCast {
    left: Box<Expr>,
    to_type: ValType,
    operator: Token,
}

impl TypeCast {
    pub(crate) fn new(left: Expr, to_type: ValType, operator: Token) -> Self {
        Self {
            left: Box::new(left),
            to_type,
            operator,
        }
    }

    pub fn left(&self) -> &Expr {
        &self.left
    }

    pub fn to_type(&self) -> &ValType {
        &self.to_type
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    expr: Box<Expr>,
}

impl Grouping {
    pub(crate) fn new(expr: Expr) -> Self {
        Self {
            expr: Box::new(expr),
        }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: Token,
}

impl Variable {
    pub(crate) fn new(name: Token) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    name: Token,
    operator: Token,
    expr: Box<Expr>,
}

impl Assignment {
    pub(crate) fn new(name: Token, operator: Token, expr: Expr) -> Self {
        Self {
            name,
            operator,
            expr: Box::new(expr),
        }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct Match {
    keyword: Token,
    expr: Box<Expr>,
    arms: Vec<MatchArm>,
    default: Option<Box<Expr>>,
}

impl Match {
    pub(crate) fn new(
        keyword: Token,
        expr: Expr,
        arms: Vec<MatchArm>,
        default: Option<Box<Expr>>,
    ) -> Self {
        Self {
            keyword,
            expr: Box::new(expr),
            arms,
            default,
        }
    }

    pub fn keyword(&self) -> &Token {
        &self.keyword
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn arms(&self) -> &[MatchArm] {
        &self.arms
    }

    pub fn default(&self) -> &Option<Box<Expr>> {
        &self.default
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    expr: Box<Expr>,
    body: Box<Expr>,
}

impl MatchArm {
    pub(crate) fn new(expr: Expr, body: Expr) -> Self {
        Self {
            expr: Box::new(expr),
            body: Box::new(body),
        }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }
}
