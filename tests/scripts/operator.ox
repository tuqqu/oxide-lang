// math

let x = 10 + 10;
dbg(x);

let x = 24 + (-3);
dbg(x);

let x = 10 - 10;
dbg(x);

let x = 24 - (-3);
dbg(x);

let x = 10 * 10;
dbg(x);

let x = 24 * (-3);
dbg(x);

let x = 10 / 10;
dbg(x);

let x = 24 / (-3);
dbg(x);

let x = 20 % 6;
dbg(x);

let x = 24 % (-3);
dbg(x);

// math assignment

let mut x = 10;
x += 5;
dbg(x);

let mut x = 10;
x -= 5;
dbg(x);

let mut x = 10;
x /= 5;
dbg(x);

let mut x = 10;
x *= 5;
dbg(x);

let mut x = 10;
x %= 5;
dbg(x);

// concat

let mut x = "hello" + " world";
dbg(x);

let x = "this is ten: " + 10 as str;
dbg(x);

let mut x = "hello";
x += " world";
dbg(x);

// equality

let x = 8 == 8;
dbg(x);

let x = "hello" == "world";
dbg(x);

let x = "hello" != "hi";
dbg(x);

let x = true == true;
dbg(x);

let x = false == true;
dbg(x);

let x = nil == nil;
dbg(x);

// logic

let x = true && true;
dbg(x);

let x = true && false;
dbg(x);

let x = false && true;
dbg(x);

let x = false && false;
dbg(x);

let x = true || true;
dbg(x);

let x = true || false;
dbg(x);

let x = false || true;
dbg(x);

let x = false || false;
dbg(x);

// comparison

let x = 50 > -4;
dbg(x);

let x = 4 >= 4;
dbg(x);

let x = 4 < 2;
dbg(x);

let x = 4 <= 0;
dbg(x);

// bitwise

let x = 10 ^ 45;
dbg(x);

let x = 42 | 9;
dbg(x);

let x = 67 & 9;
dbg(x);

let mut x = 10;
x ^= 34;
dbg(x);

let mut x = 42;
x |= 34;
dbg(x);

let mut x = 67;
x &= 21;
dbg(x);

// mix

let x = 45 * 59 + 34 ^ (499 - 34 * 1 & 3) - 34 - (-0) + (32 & 34 | 3) / 344 + 4 % 4 ^ 2;
dbg(x);

let r = false;
let x = true || true || false || (true || 8 <= 100) || r;
dbg(x);

let x = true && "hello" == ("he" + "llo") && (3 == 3) && ((6 * 6 / 6) == (1 * 3 + 2 + 1)) && 6 != 5 || false;
dbg(x);

let x = 8 != 5 && ((1)) * 78 > 100;
dbg(x);

let x = 2 >= 56 && r;
dbg(x);

let x = 2 >= 56 && 443 ^ 234 == 443 ^ 234 & 342 && false || 34 == 34;
dbg(x);
