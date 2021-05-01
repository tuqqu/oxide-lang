for x in vec[10, 20, 30] {
    dbg(x);
}

for name in vec["John", "Johann", "Jane"] {
    dbg(name);
}

let values = vec[44, 55, 66];
for value in values {
    dbg(value);
}

struct X {
    pub vals: vec<str>
}

let x = X { vals: vec["a", "b", "c", "d"] };
for x in x.vals {
    dbg(x);
}

for x in x.vals {
    if x == "a" {
        continue;
    }
    dbg(x);
}

dbg(x); // value "x" must not be overwritten by the for in loops

for x in x.vals {
    if x == "c" {
        break;
    }
    dbg(x);
}

let i = true;
for i in vec[100, 200, 300, 400] {
    dbg(i);
    if i == 200 {
        for i in vec["hello", "hi", "good day"] {
            dbg(i);
            if i == "hi" {
                loop {
                    break;
                }
                break;
            }
        }
    }
}
dbg(i);


