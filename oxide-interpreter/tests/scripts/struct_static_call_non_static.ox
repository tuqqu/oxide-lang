struct Circle {
    radius: float
}

impl Circle {
    pub fn larger_circle(self, by: float) -> Self {
        return Self {
            radius: self.radius + by
        };
    }

    pub fn to_str(self) -> str {
        return "radius: " + self.radius as str;
    }
}

let c = Circle {
    radius: 10
};

let larger = Circle::larger_circle(c, 5);
dbg(larger);

let radius = Circle::to_str(c);
dbg(radius);
