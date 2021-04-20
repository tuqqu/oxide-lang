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
		return ("Circle(radius: " + self.radius as str + ")");
	}
}

let c = Circle {
	radius: 10
};

println(c.larger_circle(10).to_str());
