use crate::expr::{Expr, Lambda, Param};
use crate::valtype::ValType;
use crate::Token;

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
    ForInStmt(ForIn),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    name: Token,
    init: Box<Option<Expr>>,
    v_type: Option<ValType>,
    mutable: bool,
}

impl VarDecl {
    pub(crate) fn new(
        name: Token,
        init: Option<Expr>,
        mutable: bool,
        v_type: Option<ValType>,
    ) -> Self {
        Self {
            name,
            init: Box::new(init),
            v_type,
            mutable,
        }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn init(&self) -> &Option<Expr> {
        &self.init
    }

    pub fn v_type(&self) -> &Option<ValType> {
        &self.v_type
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    name: Token,
    init: Box<Expr>,
    v_type: Option<ValType>,
}

impl ConstDecl {
    pub(crate) fn new(name: Token, init: Expr, v_type: Option<ValType>) -> Self {
        Self {
            name,
            init: Box::new(init),
            v_type,
        }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn init(&self) -> &Expr {
        &self.init
    }

    pub fn v_type(&self) -> &Option<ValType> {
        &self.v_type
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    stmts: Vec<Stmt>,
}

impl Block {
    pub(crate) fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
}

#[derive(Debug, Clone)]
pub struct If {
    condition: Box<Expr>,
    then_stmt: Box<Stmt>,
    else_stmt: Box<Stmt>,
}

impl If {
    pub(crate) fn new(condition: Expr, then_stmt: Stmt, else_stmt: Stmt) -> Self {
        Self {
            condition: Box::new(condition),
            then_stmt: Box::new(then_stmt),
            else_stmt: Box::new(else_stmt),
        }
    }

    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn then_stmt(&self) -> &Stmt {
        &self.then_stmt
    }

    pub fn else_stmt(&self) -> &Stmt {
        &self.else_stmt
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    #[allow(dead_code)]
    keyword: Token,
    expr: Box<Expr>,
}

impl Return {
    pub(crate) fn new(keyword: Token, expr: Expr) -> Self {
        Self {
            keyword,
            expr: Box::new(expr),
        }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    name: Token,
    lambda: Lambda,
}

impl FnDecl {
    pub(crate) const MAX_ARGS: usize = 127;

    pub(crate) fn new(name: Token, lambda: Lambda) -> Self {
        Self { name, lambda }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn lambda(&self) -> &Lambda {
        &self.lambda
    }
}

#[derive(Debug, Clone)]
pub struct FnSignatureDecl {
    name: Token,
    params: Vec<Param>,
    ret_type: ValType,
}

impl FnSignatureDecl {
    pub(crate) fn new(name: Token, params: Vec<Param>, ret_type: ValType) -> Self {
        Self {
            name,
            params,
            ret_type,
        }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn params(&self) -> &[Param] {
        &self.params
    }

    pub fn ret_type(&self) -> &ValType {
        &self.ret_type
    }
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    /// Enum name.
    name: Token,
    /// Vector of values.
    vals: Vec<Token>,
}

impl EnumDecl {
    pub(crate) fn new(name: Token, vals: Vec<Token>) -> Self {
        Self { name, vals }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn vals(&self) -> &[Token] {
        &self.vals
    }
}

type StructItem<T> = (T, bool);

#[derive(Debug, Clone)]
pub struct StructDecl {
    /// Struct name.
    name: Token,
    /// Vector of properties.
    props: Vec<StructItem<VarDecl>>,
}

impl StructDecl {
    pub(crate) fn new(name: Token, props: Vec<StructItem<VarDecl>>) -> Self {
        Self { name, props }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn props(&self) -> &[StructItem<VarDecl>] {
        &self.props
    }
}

#[derive(Debug, Clone)]
pub struct ImplDecl {
    /// Impl name.
    impl_name: Token,
    /// For name if present.
    for_name: Option<Token>,
    /// Instance methods.
    methods: Vec<StructItem<FnDecl>>,
    /// Static methods.
    fns: Vec<StructItem<FnDecl>>,
    /// Associated constants.
    consts: Vec<StructItem<ConstDecl>>,
}

impl ImplDecl {
    pub(crate) fn new(
        impl_name: Token,
        for_name: Option<Token>,
        methods: Vec<StructItem<FnDecl>>,
        fns: Vec<StructItem<FnDecl>>,
        consts: Vec<StructItem<ConstDecl>>,
    ) -> Self {
        Self {
            impl_name,
            for_name,
            methods,
            fns,
            consts,
        }
    }

    pub fn impl_name(&self) -> &Token {
        &self.impl_name
    }

    pub fn for_name(&self) -> &Option<Token> {
        &self.for_name
    }

    pub fn methods(&self) -> &[StructItem<FnDecl>] {
        &self.methods
    }

    pub fn fns(&self) -> &[StructItem<FnDecl>] {
        &self.fns
    }

    pub fn consts(&self) -> &[StructItem<ConstDecl>] {
        &self.consts
    }
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    /// Trait name.
    name: Token,
    /// Instance method signatures
    method_signs: Vec<FnSignatureDecl>,
}

impl TraitDecl {
    pub(crate) fn new(name: Token, method_signs: Vec<FnSignatureDecl>) -> Self {
        Self { name, method_signs }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn method_signs(&self) -> &[FnSignatureDecl] {
        &self.method_signs
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    inc: Box<Expr>,
    condition: Box<Expr>,
    body: Box<Stmt>,
}

impl Loop {
    pub(crate) fn new(inc: Expr, condition: Expr, body: Stmt) -> Self {
        Self {
            inc: Box::new(inc),
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    pub fn inc(&self) -> &Expr {
        &self.inc
    }

    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn body(&self) -> &Stmt {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct ForIn {
    iter_value: Token,
    index_value: Option<Token>,
    iter: Box<Expr>,
    /// Body of the loop.
    /// We store Block directly instead of the generic Stmt
    /// Because we need to evaluate it with a custom env
    body: Block,
}

impl ForIn {
    pub(crate) fn new(
        iter_value: Token,
        index_value: Option<Token>,
        iter: Expr,
        body: Block,
    ) -> Self {
        Self {
            iter_value,
            index_value,
            iter: Box::new(iter),
            body,
        }
    }

    pub fn iter_value(&self) -> &Token {
        &self.iter_value
    }

    pub fn index_value(&self) -> &Option<Token> {
        &self.index_value
    }

    pub fn iter(&self) -> &Expr {
        &self.iter
    }

    pub fn body(&self) -> &Block {
        &self.body
    }
}
