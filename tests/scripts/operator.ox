// math

let x = 10 + 10;
println(x);

let x = 24 + (-3);
println(x);

let x = 10 - 10;
println(x);

let x = 24 - (-3);
println(x);

let x = 10 * 10;
println(x);

let x = 24 * (-3);
println(x);

let x = 10 / 10;
println(x);

let x = 24 / (-3);
println(x);

let x = 20 % 6;
println(x);

let x = 24 % (-3);
println(x);

// math assignment

let mut x = 10;
x += 5;
println(x);

let mut x = 10;
x -= 5;
println(x);

let mut x = 10;
x /= 5;
println(x);

let mut x = 10;
x *= 5;
println(x);

let mut x = 10;
x %= 5;
println(x);

// concat

let mut x = "hello" + " world";
println(x);

let x = "this is ten: " + 10 as str;
println(x);

let mut x = "hello";
x += " world";
println(x);

// equality

let x = 8 == 8;
println(x);

let x = "hello" == "world";
println(x);

let x = "hello" != "hi";
println(x);

let x = true == true;
println(x);

let x = false == true;
println(x);

let x = nil == nil;
println(x);

// logic

let x = true && true;
println(x);

let x = true && false;
println(x);

let x = false && true;
println(x);

let x = false && false;
println(x);

let x = true || true;
println(x);

let x = true || false;
println(x);

let x = false || true;
println(x);

let x = false || false;
println(x);

// comparison

let x = 50 > -4;
println(x);

let x = 4 >= 4;
println(x);

let x = 4 < 2;
println(x);

let x = 4 <= 0;
println(x);

// bitwise

let x = 10 ^ 45;
println(x);

let x = 42 | 9;
println(x);

let x = 67 & 9;
println(x);

let mut x = 10;
x ^= 34;
println(x);

let mut x = 42;
x |= 34;
println(x);

let mut x = 67;
x &= 21;
println(x);

// mix

let x = 45 * 59 + 34 ^ (499 - 34 * 1 & 3) - 34 - (-0) + (32 & 34 | 3) / 344 + 4 % 4 ^ 2;
println(x);

let r = false;
let x = true || true || false || (true || 8 <= 100) || r;
println(x);

let x = true && "hello" == ("he" + "llo") && (3 == 3) && ((6 * 6 / 6) == (1 * 3 + 2 + 1)) && 6 != 5 || false;
println(x);

let x = 8 != 5 && ((1)) * 78 > 100;
println(x);

let x = 2 >= 56 && r;
println(x);

let x = 2 >= 56 && 443 ^ 234 == 443 ^ 234 & 342 && false || 34 == 34;
println(x);
