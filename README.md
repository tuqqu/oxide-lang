# Oxide Programming Language
Interpreted scripting language with a Rust influenced syntax. [Latest release][latest-releases]

## Example programs

```rust
/// structs

struct Circle {                                 // struct declaration
    pub radius: float,                          // public field
    center: Point,                              // private field
}

impl Circle {                                   // struct implementation
    const PI = 3.14159;                         // private associated constant

    pub fn new(r: float, c: Point) -> Self {    // public static method
        return Self {
            radius: r,
            center: c,
        };
    }
}

struct Point {
    pub x: int,
    pub y: int,
}

/// traits

trait Shape {                                   // trait declaration
    fn calc_area(self) -> float;                // trait methods are always public
}

impl Shape for Circle {                         // trait implementation
    fn calc_area(self) -> float {  
        return Self::PI * self.radius * self.radius;
    }
}

// program entry point
fn main() {                                     
    let a = Circle::new(200.0, Point { x: 1, y: 5 });
    let area = a.calc_area();                   
  
    println("circle area: " + area as str);     // type casting
}

```

```rust
/// enums

enum Ordering {
    Less,
    Equal,
    Greater
}

impl Ordering {
    pub fn compare(a: int, b: int) -> Self {
        return match true {
            a < b  => Self::Less,
            a == b => Self::Equal,
            a > b  => Self::Greater,
        };
    }
}

fn main() {
    let order = Ordering::compare(10, 5); // Ordering::Greater
}
```

```rust
/// sorting a vector using
/// insertion sort

fn insertion_sort(input: vec<int>) {
    for let mut i = 1; i < input.len(); i += 1 {
        let cur = input[i];
        let mut j = i - 1;
    
        while input[j] > cur {
            let temp = input[j + 1];
            input[j + 1] = input[j];
            input[j] = temp;
      
            if j == 0 {
                break;
            }
    
            j -= 1;
        }
    }
}

fn main() {
    let input: vec<int> = vec[4, 13, 0, 3, -3, 4, 19, 1];
    insertion_sort(input);
    dbg(input); // [vec] [-3, 0, 1, 3, 4, 4, 13, 19]
}

```

[More examples][examples]

## Usage

Download the [latest release][latest-releases] and put the executable in your PATH.

```
USAGE:
    oxide [FLAGS] [FILE]

FLAGS:
    -h, --help              Prints help
    -v, --version           Prints version
    -r, --repl              Run REPL
    -t, --allow-top-level   Allow top-level instructions

ARGS:
    <FILE>  Script file to run

EXAMPLE:
    oxide script.ox
```

## Building from source
If your architecture is not supported by the pre-built binaries you can build the interpreter from the source code yourself. Make sure you have Rust installed.

```shell
git clone https://github.com/tuqqu/oxide-lang.git
cd oxide-lang
cargo install --path . # copies binary to /.cargo/bin/
                       # to uninstall run `cargo uninstall`

# you can now run it with
oxide script.ox
```

# Quick Overview

* [Program Structure](#program-structure)
* [Variables and Type System](#variables-and-type-system)
    * [Mutability](#mutability)
    * [Shadowing](#shadowing)
    * [Casting](#casting)
* [Control Flow and Loops](#control-flow-and-loops)
    * [If](#if)
    * [Match](#match)
    * [While](#while)
    * [Loop](#loop)
    * [For](#for)
* [Functions](#functions)
    * [Closures](#closures)
* [Structs](#structs)
  * [Public and Private](#public-and-private)
* [Traits](#traits)
* [Enums](#enums)
* [Vectors](#vectors)
* [Constants](#constants)
* [Operators](#operators)
    * [Unary](#unary)
    * [Binary](#binary)
* [Comments](#comments)
* [Standard library](#standard-library)

## Program Structure

In Oxide, the entry point of a program is a function named `main`.

```rust
fn main() {
    // code goes here
}
```

On the top level only item (`const`, `fn`, `struct`, `enum`, `trait`) declarations are possible.

```rust
const C = 0;
struct S {}
trait T {}
enum E {}
```

## Variables and Type System

There are eleven types in Oxide: `nil`, `num`, `int`, `float`, `bool`, `str`, `fn(T) -> T`, `vec<T>`, `any` and user-defined types (via [`structs`](#structs) and [`enums`](#enums)). See [type system][type-system]

Variables are typed either explicitly:

```rust
let x: int;                                  // type = int
let y: str = "hello world";                  // type = str
let nums: vec<int> = vec[1, 2, 3];           // type = vec<int>
let jane: Person = Person { name: "Jane" };  // type = Person

// functions are their own type
let double: fn(int) -> int = fn (x: int) -> int { return x * 2; };
          //^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          //     type                      lambda       
```

or their type implicitly inferred:

```rust
let x = 1;                         // inferred as int
let dog = Dog::new("Good Boy");    // inferred as Dog
let ordering = Ordering::Less;     // inferred as Ordering
let f = fn (x: int) { ... };       // inferred as fn(int)

let x;                             // inferred as vec<int>
x = vec[1, 2, 3];                  // the first time it is being assigned
```

### Mutability

Variables are immutable by default:

```rust
let x = 100;
x += 1; //! error, x is immutable
```

To be mutable they must be defined with `mut` keyword.

```rust
let mut x: str = "hello";
x += " world"; // ok
```

### Shadowing

Variables can be shadowed. Each variable declaration "shadows" the previous one:

```rust
let x: int = 100;
let x: vec<int> = vec[1, 2];
```

### Casting

Explicit type conversion, i.e. type casting, can be performed using the `as` keyword.
Primitive types `int`, `float`, `nil`, `bool`, `str` can be cast to other primitive types.

```rust
let x = 32 as str;              // typeof(x) = str, x = "32"
let x = "350" as int;           // typeof(x) = int, x = 350
let x = 0.0 as bool;            // typeof(x) = bool, x = false

let x = 10;
"this is x: " + x as str;       // values must be cast to str for concatenation
```

Vector, enum, function and struct types cannot be used in type casting.

```rust
let x = Ordering::Less as int;  //! type error
let x = vec[] as Ordering;      //! type error
```

`any` type must be explicitly cast to be used in expressions:

```rust
let x: any = 1;
let d = 100 + x as int; // omitting cast would produce an error
```

## Control Flow and Loops

### If

`if` statement is pretty classic. It supports `else if` and `else` branches. Parentheses are not needed around conditions. Each branch must be enclosed in curly braces.

```rust
if x >= 100 {
    println("x is more than 100");
} else if x <= 100 && x > 0 {
    println("x is less than 100, but positive");
} else {
    println("x a non-positive number");
}
```

### Match

`match` expression returns the first matching arm evaluated value. Unlike other control flow statements, `match` is an expression and therefore must be terminated with a semicolon. It can be used in any place an expression is expected.

```rust
let direction = match get_direction() {
    "north" => 0,
    "east" => 90,
    "south" => 180,
    "west" => 270,
};
```

`match true` can be used to make more generalised comparisons.
```rust
let age = 40;

let description: str = match true {
    age > 19 => "adult",
    age >= 13 && x <= 19 => "teenager",
    age < 13 => "kid",
};
```

`match` can be used with [enums](#enums)

```rust
enum HttpStatus {
    NotFound,
    NotModified,
    Ok
}

impl HttpStatus {
    fn code(status: Self) -> int {
        return match status {
            Self::NotFound => 404,
            Self::NotModified => 304,
            Self::Ok => 200,
        };
    }
}

fn main() {
    let status = HttpStatus::code(HttpStatus::Ok); // 200
}
```

### While

There are three loops in Oxide: `while`, `loop` and `for`. 

Loops support `break` and `continue` statements. Loop body must be enclosed in curly braces.

`while` statement is rather usual.

```rust
while x != 100 {
    x += 1;
}
```

### Loop

`loop` looks like Rust's `loop`. Basically it is `while true {}` with some nice looking syntax.

```rust
loop {
    x.push(0);
    if x.len() > 100 {
        break;
    }
}
```

### For

`for ... in` loops are used to iterate over a vector.

```rust
for name in vec["John", "Johann", "Jane"] {
    println(name);
}
```

There is also a ~~good~~ old C-like `for` loop.
```rust
for let mut i = 0; i < v.len(); i += 1 {
    println(v[i] as str);
}
```

Like in C, the first or the last parts can be omitted, or even all three of them `for ;; {}`.

## Functions

Functions are declared with a `fn` keyword. 

Function signature must explicitly list all argument types as well as a return type.

Functions that do not have a `return` statement implicitly return `nil` and the `-> nil` may be omitted.

Each function is of `fn(T, [T,]) -> T` type.

```rust
fn add(x: int, y: int) -> int {  // typeof(add) = fn(int, int) -> int
    return x + y;
}

fn clone(c: Circle) -> Circle {  // typeof(clone) = fn(Circle) -> Circle
    return Circle {
        radius: c.radius,
        center: c.center,
    };
}

fn log(level: int, msg: str) {   // typeof(log) = fn(int, str) 
    println(
      "Level: " + level as str + ", message: " + msg
    );
}
```

Defining a function argument as `mut` lets you mutate it in the function body. By default, it is immutable.

```rust
/// compute the greatest common divisor 
/// of two integers using Euclids algorithm

fn gcd(mut n: int, mut m: int) -> int {   // typeof(gcd) = fn(int, int) -> int
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

### Closures

Functions are first-class citizens of the language, they can be stored in a variable of type `fn(T) -> T`, passed to or/and returned from another function.

```rust
/// function returns closure
/// which captures the internal value i
/// each call to the closure increments the captured value
fn create_counter() -> fn() {       // typeof(create_counter) = fn() -> fn()
    let mut i = 0;

    return fn () {                  // returns closure
        i += 1;
        println(i as str);
    };
}

let counter = create_counter();     // inferred as fn() -> fn()

counter(); // 1
counter(); // 2
counter(); // 3
```

Functions can be passed by their name directly

```rust
fn str_concat(a: str, b: str) -> str {
    return a + b;
}

fn str_transform(
    callable: fn(str, str) -> str, 
    a: str,
    b: str
) -> str {
    return callable(a, b);
}

str_transform(str_concat, "hello", " world");
```

Immediately Invoked Function Expressions, short IIFE, are also supported for whatever reason.

```rust
(fn (names: vec<str>) {
    for name in names {
        println(name);
    }
})(vec["Rob", "Sansa", "Arya", "Jon"]);
```

## Structs

Structs represent the user-defined types. Struct declaration starts with `struct` keyword.
All struct properties are mutable by default.

You can make property public with a `pub` keyword. Public fields can be accessed from outside scope.

```rust
struct StellarSystem {           
    pub name: str,               // public field
    pub planets: vec<Planet>,    // public vector of structs
    pub star: Star,         
    age: int                     // private field
}

struct Star { 
    pub name: str,
    mass: int,                  
}

struct Planet {
    pub name: str,
    mass: int,
    belt: bool,
}
```

Struct implementation starts with `impl` keyword.
While struct declaration defines its properties, struct implementation defines its methods, static methods and constants.

* `self` keyword can be used inside methods and points to the current struct instance. i.e `self.field`
* `Self` (capitalised) keyword can be used inside methods to point to the current struct name, i.e. `Self::CONSTANT` or `Self::static_method(x)`, it can be used as a type as well `let p: Self = Self { .. };`

You can make methods and constants public with a `pub` keyword. Public methods and constants can be accessed from outside scope.

Methods with `self` as the first argument are instance methods. Methods without it are static methods. 

```rust
impl Star {                      
    pub const WHITE_DWARF = 123.3;     // public associated constants
    pub const NEUTRON_STAR = 335.2;    // lets pretend those values are real
    pub const BLACK_HOLE = 9349.02;  
  
    const MAX_AGE = 99999;             // private constant
  
    pub fn new(name: str, mass: int) -> Self {  // public static method
        return Self {
            name: name,
            mass: mass,
        }
    }
    
    pub fn get_description(self) -> str {  // public instance method
        return match true {
            self.mass <= Self::WHITE_DWARF => "white dwarf",
            self.mass > Self::WHITE_DWARF && self.mass <= Self::BLACK_HOLE => "neutron star",
            self.mass >= Self::BLACK_HOLE => "black hole",
        };
    }
}

impl Planet {
    pub fn set_new_mass(self, mass: int) {
        self.mass = mass;
    }

    fn is_heavier(self, p: Self) -> bool {      // private method
        return self.mass > p.mass;
    }
}
```

You need to initialize all structs properties on instantiation.

```rust
let planet_mars: Planet = Planet {                   
    name: "Mars",
    mass: 100,
    belt: false,
};

let system = StellarSystem {
    name: "Solar System",
    star: Star { name: "Sun", mass: 9999 },
    planets: vec[
        planet_mars,                                        // via variable
        Planet { name: "Earth", belt: false, mass: 120 }    // via inlined struct instantiation
    ],    
    age: 8934,
};

let arcturus = Planet::new("Arcturus", 4444);               // creating instance via static method
```

Dot syntax `.` is used to access structs fields and call its methods.

```rust
// set new value
system.name = "new name";
system.planets.push( Planet {
    name: "Venus",
    belt: false,
    mass: 90,
});

// get value
let mars_name: str = system.planets[0].name;

// call method
let desc: str = system.star.get_description();
system.planets[0].set_new_mass(200);
```

`::` is used to access constants and static methods:
```rust
impl Star {
    pub const WHITE_DWARF = 123.3;
    const MAX_AGE = 99999; 
    
    // inside methods Self:: can be used instead
    pub fn get_max_age() -> int {
        return Self::MAX_AGE;
    }
}

let dwarf_mass: int = Star::WHITE_DWARF;
let max_age: int = Star::get_max_age();
```

Same as in Rust, non-static methods can be called using `::` as well:
```rust
let desc = Star::get_description(system.star);
// same as
let desc = system.star.get_description();
```

Immutable variable behave similarly to Javascript `const` that holds an object, so it will still let you change the object fields.

```rust
system.name = "new name";              // valid
system.planets[0].name = "new name";   // also valid
system = StellarSystem { ... };        //! error, "system" cannot point to another struct
```

Structs are always passed by reference, consider:

```rust
fn remove_planets(s: StellarSystem) {
    s.planets = vec<Planet>[];        // oh, all planets are removed
}

remove_planets(system);
```
### Public and Private

Only public properties, methods and constants can be accessed from outside code.

```rust
system.age = 100;                //! access error, "age" is private
system.planets[0].mass;          //! access error, "mass" is private
system.planets[0].is_heavier(p); //! access error, "is_heavier()" is private
Star::MAX_AGE;                   //! access error, "Star::MAX_AGE" is private
```

## Traits

Traits are similar to Rust traits and are used to define shared behavior.

Because all trait methods are always public they are defined with no `pub` keyword.

_Note: no static methods allowed._

```rust
trait Shape {
    fn calc_area(self) -> float;
  
    fn calc_perimeter(self) -> float;
}
```

Trait body lists function signatures that must be implemented when implementing the trait:

```rust
impl Shape for RightTriangle {
    fn calc_area(self) -> float {
        return self.a * self.b / 2;
    }
  
    fn calc_perimeter(self) -> float {
        return self.a + self.b + self.c;
    }
}

impl Shape for Rectangle {
    fn calc_area(self) -> float {
        return self.a * self.b;
    }
  
    fn calc_perimeter(self) -> float {
        return (self.a + self.b) * 2;
    }
}
```

All structs that implement the `Shape` trait can be used wherever a shape is expected:

```rust
fn print_shape_values(shape: Shape) {
    let area = shape.calc_area();
    let perimeter = shape.calc_perimeter();

    println("The area is " + area as str);
    println("The perimeter is " + perimeter as str);
}

let a = Rectangle::new(10, 30);
print_shape_values(a); 
```

## Enums

Same as structs, enums represent user-defined types. Enums are quite simple and similar to C-style enums.

```rust
enum TimeUnit {
    Seconds,
    Minutes,
    Hours,
    Days,
    Months,
    Years,
}

let days = TimeUnit::Days; // inferred type as "TimeUnit"
```

`impl` blocks can be used to implement static methods and constants on enums. 

_Future scope: allow instance methods on enums._

```rust
impl TimeUnit {   
    pub fn plural(time: Self) -> str {
        return match time {
            Self::Seconds => "seconds",
            Self::Minutes => "minutes",
            Self::Hours => "hours",
            Self::Days => "days",
            Self::Months => "months",
            Self::Years => "years"
        };
    }
}

TimeUnit::plural(days); // "days"
```

Equality of enum values can be checked with `==` and `!=` or with `match`

```rust
days == TimeUnit::Days;             // true
TimeUnit::Years == TimeUnit::Hours; // false
```

Different types of enum values are not compatible and comparing them will trigger an error

```rust
enum Ordering {
    Less,
    Equal,
    Greater
}

Ordering::Less == TimeUnit::Days; //! type error
```

## Vectors

Vectors, values of type `vec<type>`, represent arrays of values and can be created using `vec<type>[]` syntax, where `type` is any Oxide type.

Vectors have built-in methods:
* `vec.push(val: any)` push value to the end of the vector
* `vec.pop() -> any` remove value from the end of the vector and return it
* `vec.len() -> int` get vectors length

```rust
let planets = vec["Mercury", "Venus", "Earth", "Mars"];

planents.push("Jupiter");    // "Jupiter" is now the last value in a vector
let jupiter = planets.pop(); // "Jupiter" is no longer in a vector.

let mars = planets[3];       // mars = "Mars"
planets[2] = "Uranus";       // "Earth" is gone. "Uranus" is on its place now

planets.len();               // 3

typeof(planets);             // vec<str>
```

When type is omitted it is inferred as `any` on type declaration, but on vector instantiation it is inferred as proper type **when possible**.
Consider:
```rust
let v: vec = vec[];                     // typeof(v) = vec<any>
let v = vec[];                          // typeof(v) = vec<any>

let v: vec<int> = vec[1, 2, 3];         // typeof(v) = vec<int>
let v = vec<int>[1, 2, 3];              // typeof(v) = vec<int>
let v = vec[1, 2, 3];                   // typeof(v) = vec<int>, because all the initial values are of type "int"

let v = vec<bool>[true];                // typeof(v) = vec<bool>

let v: vec<Dog> = vec[                  // typeof(v) = vec<Dog>, type declaration can actually be omitted
    Dog { name: "dog1" },
    Dog { name: "dog2" },
];

let v = vec[                            // typeof(v) = vec<vec<Point>,
    vec[                                // inferred by the initial values, despite the type being omitted
        Point { x: 1, y: 1 }, 
        Point { x: 0, y: 3 } 
    ],
    vec[ 
        Point { x: 5, y: 2 }, 
        Point { x: 3, y: 4 } 
    ],
];


let matrix = vec[                       // typeof(v) = vec<vec<int>>
    vec[1, 2, 3, 4, 5],
    vec[3, 4, 5, 6, 7],
    vec[3, 4, 5, 6, 7],
];

let things = vec[nil, false, Dog {}];   // typeof(v) = vec<any>
```

Like structs vectors are passed by reference. 

Consider this example of an in place sorting algorithm, selection sort, that accepts a vector and sorts it in place, without allocating memory for a new one.

<details>
  <summary>Selection sort</summary>

```rust
fn selection_sort(input: vec<int>) {
    if input.len() == 0 {
        return;
    }

    let mut min: int;
    for let mut i = 0; i < input.len() - 1; i += 1 {
        min = i;

        for let mut j = i; j < input.len(); j += 1 {
            if input[j] < input[min] {
                min = j;
            }
        }

        if min != i {
            let temp = input[i];
            input[i] = input[min];
            input[min] = temp;
        }
    }
}
```
</details>

Vectors can hold any values and be used inside structs.
```rust
let animals = vec[
    Elephant { name: "Samuel" },
    Zebra { name: "Robert" },
    WolfPack { 
        wolves: vec[
            Wolf { name: "Joe" },
            Wolf { name: "Mag" },
        ]
    }
]

let mag = animals[2].wolves[1]; // Mag
```

Trying to read from a non-existent vector index will result in a `uninit` value returned. Trying to write to it will result in an error.

## Constants

Constants unlike variables need not be declared with a type since it can always be inferred. Constants also cannot be redeclared, and it will result in a runtime error. Constants **must** hold only a scalar value: `str`, `int`, `float`, `bool`.

```rust
const MESSAGE = "hello world";

const SOME_THRESHOLD = 100;

const EPSILON = 0.004;
```

Struct implementations (`impl` blocks) can also define constants. Those constants can be either public or private.
```rust
struct Math {}

impl Math {
    pub const PI = 3.14159265;  // public const
    const E = 2.71828182846;    // private const
  
    pub fn get_e() -> float {
        return Self::E;         // Self:: is the same as Math:: inside methods
    }
}
```

Accessing private consts from outer scope will result in an error.

```rust
let pi = Math::PI;     // ok
let e = Math::E;       //! access error
let e = Math::get_e(); // ok
```

## Operators

### Unary
- `!` negates boolean value
- `-` negates number

### Binary
- `&&`, `||` logic, operate on `bool` values
- `<`, `>`, `<=`, `>=`, comparison, operate on `int`, `float` values
- `==`, `!=` equality, operate on values of the **same** type
- `-`, `/`, `+`, `*`, `%` math operations on on `int`, `float` values
- `&`, `|`, `^` bitwise operations on integers
- `+` string concatenation
- `as` type cast operator, used to convert primitives to some type: `30 as bool`
- `=`, `+=`, `-=`, `/=`, `%=`, `*=`, `&=`, `|=`, `^=` various corresponding assignment operators

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

A small set of built-in functionality is available anywhere in the code.

- `typeof(val: any) -> str` returns type of given value or variable
- `dbg(val: any)` dumps `val` as a string to the standard output stream (stdout).
- `print(msg: str)` prints `msg` to the stdout.
- `println(msg: str)` same as `print`, but inserts a newline at the end of the string.
- `eprint(err: str)` prints `err` to the standard error (stderr).
- `eprintln(err: str)` you got the idea.
- `timestamp() -> int` returns current Unix Epoch timestamp in seconds
- `read_line() -> str` reads user input from standard input (stdin) and returns it as a `str`
- `file_write(file: str, content: str) -> str` write `content` to a file, creating it first, should it not exist


[latest-releases]: https://github.com/tuqqu/oxide-lang/releases/latest
[examples]: https://github.com/tuqqu/oxide-lang/tree/master/examples
[type-system]: /docs/type_system.md