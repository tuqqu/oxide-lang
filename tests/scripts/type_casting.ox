// int
let x = 1 as int;
dbg(typeof(x));
dbg(x);

let x = 1 as float;
dbg(typeof(x));
dbg(x);

let x = 1 as nil;
dbg(typeof(x));
dbg(x);

let x = 1 as bool;
dbg(typeof(x));
dbg(x);

let x = 100 as bool;
dbg(typeof(x));
dbg(x);

let x = 0 as bool;
dbg(typeof(x));
dbg(x);

let x = 100 as str;
dbg(typeof(x));
dbg(x);

let x = 45.45 as int;
dbg(typeof(x));
dbg(x);

// float
let x = 45.45 as float;
dbg(typeof(x));
dbg(x);

let x = 45.45 as nil;
dbg(typeof(x));
dbg(x);

let x = 1.0 as bool;
dbg(typeof(x));
dbg(x);

let x = 100.33 as bool;
dbg(typeof(x));
dbg(x);

let x = 0.0 as bool;
dbg(typeof(x));
dbg(x);

let x = 45.45 as str;
dbg(typeof(x));
dbg(x);

// nil
let x = nil as int;
dbg(typeof(x));
dbg(x);

let x = nil as float;
dbg(typeof(x));
dbg(x);

let x = nil as nil;
dbg(typeof(x));
dbg(x);

let x = nil as bool;
dbg(typeof(x));
dbg(x);

let x = nil as str;
dbg(typeof(x));
dbg(x);

// bool
let x = true as int;
dbg(typeof(x));
dbg(x);

let x = false as int;
dbg(typeof(x));
dbg(x);

let x = true as float;
dbg(typeof(x));
dbg(x);

let x = false as float;
dbg(typeof(x));
dbg(x);

let x = true as nil;
dbg(typeof(x));
dbg(x);

let x = true as bool;
dbg(typeof(x));
dbg(x);

let x = true as str;
dbg(typeof(x));
dbg(x);

let x = false as str;
dbg(typeof(x));
dbg(x);

// str
let x = "123" as int;
dbg(typeof(x));
dbg(x);

let x = "0" as int;
dbg(typeof(x));
dbg(x);

let x = "hello" as int;
dbg(typeof(x));
dbg(x);

let x = "123.38" as float;
dbg(typeof(x));
dbg(x);

let x = "0.0" as float;
dbg(typeof(x));
dbg(x);

let x = "hello" as nil;
dbg(typeof(x));
dbg(x);

let x = "hello" as bool;
dbg(typeof(x));
dbg(x);

let x = "" as bool;
dbg(typeof(x));
dbg(x);

let x = "hello" as str;
dbg(typeof(x));
dbg(x);

// any
let x: any = "123";
let x = x as int;
dbg(typeof(x));
dbg(x);

let x: any = 123;
let x = x as int;
dbg(typeof(x));
dbg(x);

let x: any = "hello";
let x = x as str;
dbg(typeof(x));
dbg(x);

let x: any = "hello";
let x = x as bool;
dbg(typeof(x));
dbg(x);
