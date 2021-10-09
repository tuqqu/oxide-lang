trait Stringable {
    fn to_str_with_prefix(self, prefix: str) -> str;
    fn to_str(self) -> str;
}

struct Circle {
    radius: float
}

struct Dot {
    x: int,
    y: int,
}

impl Stringable for Circle {
    fn to_str(self) -> str {
        return "radius: " + self.radius as str;
    }

    fn to_str_with_prefix(self, prefix: str) -> str {
        return prefix + ", radius: " + self.radius as str;
    }
}

impl Stringable for Dot {
    fn to_str(self) -> str {
        return "x: " + self.x as str + ", y: " + self.y as str;
    }

    fn to_str_with_prefix(self, prefix: str) -> str {
        return prefix + ", x: " + self.x as str + ", y: " + self.y as str;
    }
}

let c = Circle {
    radius: 10
};

let d = Dot {
    x: 10,
    y: 5,
};

let c_str = Circle::to_str(c);
dbg(c_str);

let c_str = Circle::to_str_with_prefix(c, "this is a circle ~ ");
dbg(c_str);

let d_str = Dot::to_str(d);
dbg(d_str);

let d_str = Dot::to_str_with_prefix(d, "this is a dot ~ ");
dbg(d_str);

