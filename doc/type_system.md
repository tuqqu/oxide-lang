## Type System

There are ten types `nil`, `num`, `int`, `float`, `bool`, `str`, `func`, `vec`, `any` and structs.
Type of any value can be checked by a built-in function `typeof(val: any) -> str`.

```rust
let v = vec[];
let i: int;

typeof(v); // vec
typeof(i); // int
```

### Nil
Represented by only one value, `nil` itself. Function that does not return a value implicitly returns `nil`.

### Bool
Comprises `true` and `false` values. Each logic operation, comparison or equality check produces value of type `bool`.

### Num, Float, Int
An `int` is an integer number. A `float` is floating point number.
`num` is type alias to make function accept both number types.
Numbers can be compared and used in math operations.

### Str
String is series of characters, enclosed in double quotes. Strings can be concatenated using `+` operator.

### Func
Functions can be declared with a name or created as lambda function. In that case to reuse them you should store them in a variable, immediately invoke them or pass to a function as a callable.  [See more](/README.md#functions)

### Vec
Vectors or arrays of values. [See more](/README.md#vectors)

### Struct
User-defined type. [See more](/README.md#structs)

### Enums
User-defined type. [See more](/README.md#enums)

### Any
`any` is a type alias which includes all possible types. Variable of type `any` can be reassigned to any value.

```rust
let mut x: any = "string";
x = 45.9;
x = vec[1, 2, 3, 4];
```