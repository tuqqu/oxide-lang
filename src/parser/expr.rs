use super::valtype::ValType;
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
    Enum(EnumDecl),
    Struct(StructDecl),
    Impl(ImplDecl),
    Trait(TraitDecl),
    LoopStmt(Loop),
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
pub struct SelfStatic {
    pub self_static: Token,
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
    pub token: Token,
    pub vals: Vec<Expr>,
    pub val_type: Option<ValType>,
}

#[derive(Debug, Clone)]
pub struct VecIndex {
    pub callee: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct GetStaticProp {
    pub name: Box<Expr>,
    pub prop_name: Token,
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

pub type ParamList = Vec<(Token, ValType, bool)>;

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: ParamList,
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
pub struct TypeCast {
    pub left: Box<Expr>,
    pub to_type: ValType,
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

// FIXME: try to unite FnSignature with FnDecl and Lambda
#[derive(Debug, Clone)]
pub struct FnSignatureDecl {
    pub name: Token,
    pub params: Vec<(Token, ValType, bool)>,
    pub ret_type: ValType,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    /// Enum name.
    pub name: Token,
    /// Vector of values.
    pub vals: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    /// Struct name.
    pub name: Token,
    /// Vector of properties.
    /// Bool tells if property is public or not
    pub props: Vec<(VarDecl, bool)>,
}

#[derive(Debug, Clone)]
pub struct ImplDecl {
    /// Impl name.
    pub impl_name: Token,
    /// For name if present.
    pub for_name: Option<Token>,
    /// Instance methods.
    pub methods: Vec<(FnDecl, bool)>,
    /// Static methods.
    pub fns: Vec<(FnDecl, bool)>,
    /// Associated constants.
    pub consts: Vec<(ConstDecl, bool)>,
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    /// Trait name.
    pub name: Token,
    /// Instance method signatures
    pub method_signs: Vec<FnSignatureDecl>,
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
        matches!(self, Expr::EmptyExpr)
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

impl TypeCast {
    pub fn new(left: Box<Expr>, to_type: ValType, operator: Token) -> Self {
        Self {
            left,
            to_type,
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

impl SelfStatic {
    pub fn new(self_static: Token) -> Self {
        Self { self_static }
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
    pub fn new(vals: Vec<Expr>, val_type: Option<ValType>, token: Token) -> Self {
        Self {
            vals,
            val_type,
            token,
        }
    }
}

impl VecIndex {
    pub fn new(callee: Box<Expr>, index: Box<Expr>) -> Self {
        Self { callee, index }
    }
}

impl GetStaticProp {
    pub fn new(name: Box<Expr>, prop_name: Token) -> Self {
        Self { name, prop_name }
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

impl FnSignatureDecl {
    pub fn new(name: Token, params: ParamList, ret_type: ValType) -> Self {
        Self {
            name,
            params,
            ret_type,
        }
    }
}

impl EnumDecl {
    pub fn new(name: Token, vals: Vec<Token>) -> Self {
        Self { name, vals }
    }
}

impl StructDecl {
    pub fn new(name: Token, props: Vec<(VarDecl, bool)>) -> Self {
        Self { name, props }
    }
}

impl ImplDecl {
    pub fn new(
        impl_name: Token,
        for_name: Option<Token>,
        methods: Vec<(FnDecl, bool)>,
        fns: Vec<(FnDecl, bool)>,
        consts: Vec<(ConstDecl, bool)>,
    ) -> Self {
        Self {
            impl_name,
            for_name,
            methods,
            fns,
            consts,
        }
    }
}

impl TraitDecl {
    pub fn new(name: Token, method_signs: Vec<FnSignatureDecl>) -> Self {
        Self { name, method_signs }
    }
}

impl Lambda {
    pub fn new(params: ParamList, ret_type: ValType, body: Vec<Stmt>) -> Self {
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
