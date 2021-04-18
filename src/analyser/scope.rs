pub enum Scope {
    Root(Environment),
    Function(Environment),
    Block(Environment),
}
