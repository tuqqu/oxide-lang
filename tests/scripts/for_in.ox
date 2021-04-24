for x in vec[10, 20, 30] {
    println(x);
}

for name in vec["John", "Johann", "Jane"] {
    println(name);
}

let values = vec[44, 55, 66];
for value in values {
    println(value);
}

struct X {
    pub vals: vec<str>
}

let x = X { vals: vec["a", "b", "c", "d"] };
for x in x.vals {
    println(x);
}

for x in x.vals {
    if x == "a" {
        continue;
    }
    println(x);
}

println(x); // value "x" must not be overwritten by the for in loops

for x in x.vals {
    if x == "c" {
        break;
    }
    println(x);
}

let i = true;
for i in vec[100, 200, 300, 400] {
    println(i);
    if i == 200 {
        for i in vec["hello", "hi", "good day"] {
            println(i);
            if i == "hi" {
                loop {
                    break;
                }
                break;
            }
        }
    }
}
println(i);


