## Type System

* [Nil](#nil)
* [Bool](#bool)
* [Int](#int-float)
* [Float](#int-float)
* [Str](#str)
* [Vec](#vec)
* [Struct](#struct)
* [Enums](#enums)
* [Any](#any)

See also:
* [Type Unions](#type-unions)
* [Type Declarations](#type-declarations)

There are eleven types `nil`, `int`, `float`, `bool`, `str`, `fn(T) -> T`, `vec<T>`, `any`, structs and enums.
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

```rust
fn foo() -> nil {} // same as fn foo() {}
```

### Bool
Comprises `true` and `false` values. Each logic operation, comparison or equality check produces value of type `bool`.

### Int, Float
An `int` is an integer number. A `float` is floating point number.
`num` is a type alias for `int|float`
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
`any` is a type that wraps all possible types. Variable of type `any` can be reassigned to any value.

```rust
let mut x: any = "string";
x = 45.9;
x = vec[1, 2, 3, 4];
```

`any` must be explicitly cast before used in an expression

```rust
let x: any = 10;
let y = 2 * x as int;
```

## Type Unions
Types can be compounded in unions, in this case value must satisfy at least one type.

```rust
fn x() -> str|nil {
    return "string";
}
```

## Type declarations
New type aliases can be created via the type declaration statement

```rust
type IntVec = vec<int>;

// works with unions as well
type StrOrIntVec = str|IntVec;
```

Type aliases can be used wherever a type is expected.

```rust
type StrOrNil = str|nil;

fn accept_str_or_nil(arg: StrOrNil) { ... }
```