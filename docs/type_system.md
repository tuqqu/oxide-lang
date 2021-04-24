## Type System

There are eleven types `nil`, `num`, `int`, `float`, `bool`, `str`, `fn(T) -> T`, `vec<T>`, `any`, structs and enums.
Type of any value can be checked by a built-in function `typeof(val: any) -> str`.

```rust
let v = vec[];
let i: int;

typeof(v); // vec
typeof(i); // int
typeof(
    fn (x: int) -> int {   // fn(int) -> int
        return x;
    }  
); 
```

### Nil
Represented by only one value, `nil` itself. Function that does not return a value implicitly returns `nil`.

### Bool
Comprises `true` and `false` values. Each logic operation, comparison or equality check produces value of type `bool`.

### Int, Float, Num
An `int` is an integer number. A `float` is floating point number.
`num` is type alias to make function accept both number types.
Numbers can be compared and used in math operations.

### Str
String is series of characters, enclosed in double quotes. Strings can be concatenated using `+` operator.

### Fn
Functions can be declared with a name or created as a lambda function. In that case to reuse them you should store them in a variable, immediately invoke them or pass to a function as a callable.  [See more](/README.md#functions)
Functions are of `fn(T) -> T` type.
```rust
fn inc(x: int) -> int {         // typeof(add) = "fn(int) -> int"
    return x + 1;
}

let x: fn(int) -> int = inc;    // explicit type
let x = inc;                    // inferred as "fn(int) -> int"

let f = fn() {                  // typeof(f) = "fn()"
    println("hello");           // "-> nil" is omitted
}

let f: fn() -> fn(int) -> str;  // function type can be nested
    
let f = fn (                    // inferred as "fn(int, fn(vec<str) -> str) -> str
    x: int, 
    f: fn(x: vec<str>) -> str
) -> str {      
    // ...
}
```

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