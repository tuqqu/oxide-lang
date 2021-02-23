# Oxide Programming Language
Interpreted C-like language with a Rust influenced syntax.

## Example programs

```rust
/// recursive function calls to compute n-th
/// fibonacci sequence number

fn fib(n: int) -> num {
    if n <= 1 {
        return n;
    }

    return fib(n - 2) + fib(n - 1);
}

for let mut i = 0; i < 30; i += 1 {
    println(fib(i));
}
```

```rust
/// first-class functions

let make_adder = fn (x: num) -> func {
    return fn (y: num) -> num {
        return x + y;
    };
};

let add5: func = make_adder(5);
let add7: func = make_adder(7);

println(add5(2)); // 7
println(add7(2)); // 9
```

```rust
/// structs

const PI = 3.14159;

struct Circle {
    radius: num,
    center: Point,
}

struct Point {
    x: int,
    y: int,
}

let center = Point { x: 1, y: 5 };
let circle = Circle { radius: 103.5, center: center };

fn calc_area(c: Circle) -> float {
    return PI * self.radius * self.radius;
}

calc_area(circle);
```

```rust
/// compute the greatest common divisor 
/// of two integers using Euclid’s algorithm

fn gcd(mut n: int, mut m: int) -> int {
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }

    return n;
}

gcd(15, 5); // 5
```

## Usage
### Interpret a file
```shell
oxide [script.ox]
```

### REPL

There is also a simplistic REPL mode available. 
```shell
oxide
```

# Quick Overview

* [Variables and Type System](#variables-and-type-system)
    * [Mutable Variables vs Immutable ones](#mutable-variables-vs-immutable-ones)
    * [Shadowing](#shadowing)
* [Control Flow and Loops](#control-flow-and-loops)
* [Constants](#constants)
* [Functions](#functions)
    * [Closures and Lambdas](#closures-and-lambdas)
    * [Declared functions](#declared-functions)
* [Structs](#structs)
* [Functions](#functions)
* [Operators](#operators)
    * [Unary](#unary)
    * [Binary](#binary)
* [Comments](#comments)
* [Standard library](#standard-library)


## Variables and Type System

There are eight types embodied in the language: `nil`, `num`, `int`, `float`, `bool`, `str`, `func`, `any` and user-defined types (via structs).

Each variable has a type associated with it, either explicitly declared with the variable itself:

```rust
let x: num; // accepts both integers or floats

let mut y: str = "hello" + " world";

let double: func = fn (x: num) -> num {
    return x * 2;
};
```

or implicitly inferred by the interpreter the first time it is being assigned:

```rust
let x = true || false; // x is of type "bool";

let mut y;
y = "string"; // y is of type "str";

let dog;
dog = Dog { name: "Good Boy"}; // dog is of struct-type "Dog"
```

Mutable variables cannot be assigned to a value of another type, unless they are of type `any`:

```rust
let mut s: str = "string";
s = 100; //! this is an error

let mut x = true;
x = "string"; //! this is an error, because x was inferred as "num" in initialisation

let mut a: any = Rectangle { height: 10, width: 10 };
a = 45.34; // this is valid, since it can hold a value of any type
```

### Mutable Variables vs Immutable ones

There are two possible ways of declaring a variable: immutable and mutable. Immutable ones cannot be reassigned after having been assigned to a value.

```rust
let x: str = "string";

let y: num;
y = 100;
```

However, mutable ones behave like you would expect a variable to behave in most other languages:

```rust
let mut x: str = "hello";
x += " world";
x = "another string";
```

### Shadowing

One important thing is that variables can be redeclared in other words, shadowed. Each variable declaration "shadows" the previous one and ignores its type and mutability. Consider:

```rust
let x: int = 100;
let x: str = "This was x value before: " + x;
let mut x: bool = true;
```

## Control Flow and Loops

There is an `if` statement with the support of `else if` and `else` branches. Parentheses are not needed around conditions. Each branch must be enclosed in curly braces.

```rust
if x >= 100 {
    println("x is more than 100");
} else if x <= 100 && x > 0 {
    println("x is less than 100, but positive");
} else {
    println("x a non-positive number");
}
```

There are three loops in Oxide: `while`, `loop` and `for`. All loops support `break` and `continue` statements. Loop body must be enclosed in curly braces.

`while` statement is rather usual.

```rust
while x != 100 {
    x += 1;
}
```
`loop` looks like Rust's `loop`. Basically it is `while true {}` with some nice looking syntax.

```rust
loop {
    x *= 2;
    if x > 150 {
        break;
    }
}
```

`for` loop is a ~~good~~ old C-like `for` statement, which comprises three parts. You should be familiar with it.
```rust
for let mut i = 0; i <= 100; i += 1 {
    println(i);
}

/// the first or the last parts can be omitted
let mut i = 0;

for ; i <= 100; {
    println(i);
    i += 1;
}

/// this is basically "while true" or "loop"
let mut i = 0;

for ;; {
    println(i);
    i += 1;

    if i > 100 {
        break;
    }
}
```

## Constants

Constants unlike variables need not be declared with a type since it can always be inferred. Constants also cannot be redeclared, and it will result in a runtime error. Constants **must** hold only a scalar value: `str`, `int`, `float`, `bool`.

```rust
const MESSAGE = "hello world";

const PI = 3.14159;

const E = 2,71828;
```

## Functions

### Declared functions

Functions are declared with a `fn` keyword. Function signature must explicitly list all argument types as well as a return type. Functions that do not have a return value always return `nil` and the explicit return type can be omitted (same as declaring it with `-> nil`)

```rust
fn add(x: num, y: num) -> num {
    return x + y;
}

let sum = add(1, 100); // 101

fn clone(c: Circle) -> Circle {
    return Circle {
        radius: c.radius,
        center: c.center
    };
}

let cloned = clone(circle); // cloned is now a new struct, copy of "circle"

// since this function returns nothing, the return type can be omitted
fn log(level: str, msg: str) {
    println("Level: " + level + ", message: " + msg);
}
```

Redeclaring a function will result in a runtime error.

### Closures and Lambdas

Functions can also be assigned to variables of type `func`. As with other types, it can be inferred and therefore omitted when declaring a variable.

```rust
/// function returns closure
/// which captures the internal value i
/// each call to the closure increments the captured value
fn create_counter() -> func {
    let mut i = 0;

    return fn () {
        i += 1;
        println(i);
    };
}

let counter: func = create_counter();

counter(); // 1
counter(); // 2
counter(); // 3
```

Functions are first-class citizens of the language, they can be stored in a variable, passed or/and returned from another function.

```rust
fn str_concat(prefix: str, suffix: str) -> str {
    return prefix + suffix;
}


fn str_transform(callable: func, a: str, b: str) -> any {
    return callable(s1, s2);
}

str_transform(str_concat, "hello", " world");
```

Defining a function argument as `mut` lets you mutate it in the function body. By default, it is immutable.

```rust
fn inc(mut x: int) -> int {
    x += 1;
    return x;
}

// same as
fn inc(x: int) -> int {
    let mut x = x; // shadowing argument, creating a local variable of the same name
    x += 1;
    return x;
}
```

Immediately Invoked Function Expressions, short IIFE, are also supported for whatever reason.

```rust
(fn (name: str) {
    let message = "Hello there, dear " + name + ".";
    println(message);
}("friend")
```

## Structs

Structs represent the user-defined types. The struct declaration starts with `struct` keyword.

All struct properties are mutable and public by default.

```rust
struct Person {
    first_name: str,
    last_name: str,
    alive: bool,
    pet: Animal,
    speak: func
}

struct Animal {
    kind: str,
    fluffy: bool,
}
```

You instantiate a struct creating it and initializing all its properties.

```rust
let cat = Animal {
    kind: "cat",
    fluffy: true
};

let mut john: Person = Person {
    first_name: "John",
    last_name: "Doe",
    alive: true,
    pet: cat,
    speak: fn () -> str {
        return "hello!";
    }
};
```

Immutable variables would not let you mutate the struct, so you ought to declare those you want to change in the future as `let mut`

```rust
cat.fluffy = false; //! error, cat is not a mutable variable (and cannot be *not* fluffy)

john.alive = false; // valid, John is dead now
```

## Operators

### Unary
- `!` negates boolean value
- `-` negates number 

### Binary
- `&&`, `||` logic, operate on `bool` values
- `<`, `>`, `<=`, `>=`, comparison, operate on `int`, `float` values
- `==`, `!=` equality, operate on values of the **same** type
- `-`, `/`, `+`, `*`, `%` math operations on numbers
- `+` string concatenation, also casts any other value in the same expression to `str`
- `=`, `+=`, `-=`, `/=`, `%=`, `*=` various corresponding assignment operators

## Comments

Classic comments that exist in most other languages.
```rust
// inline comments

/*
    multiline comment
 */

let x = 100; /* inlined multiline comment */ let y = x;
```

## Standard library

A small set of built-in functionality is available anywhere in the code, so I dare call it a standard library.

- `print(msg: str)` prints `msg` to the standard output stream (stdout).
- `println(msg: str)` same as `print`, but inserts a newline at the end of the string.
- `eprint(err: str)` prints `err` to the standard error (stderr).
- `eprintln(err: str)` you got the idea.
- `timestamp() -> int` returns current Unix Epoch timestamp in seconds
- `read_line() -> str` reads user input from standard input (stdin) and returns it as a `str`
- `file_write(file: str, content: str) -> str` write `content` to a file, creating it first, should it not exist
- `typeof(val: any) -> str` returns type of a given value or variable
