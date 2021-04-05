struct Planet {
    pub age: int,
    name: str,
}

impl Planet {
    pub const PUBLIC_CONST = 6;
    const PRIVATE_CONST = 10;

    pub fn get_name() -> str {
        return self.private_get_name();
    }

    fn private_get_name() -> str {
        return self.name;
    }

    pub fn set_name(new_name: str) -> nil {
        let new_name = self.private_fix_name(new_name);
        self.name = new_name;
    }

    pub fn get_private_const() -> int {
        return Planet::PRIVATE_CONST;
    }

    fn private_fix_name(name: str) -> str {
        return name + " planet";
    }
}


let mut earth = Planet { age: 10000000, name: "Earth" };

println(earth.age);

earth.age = 999999;

println(earth.age);
println(earth.get_name());

earth.set_name("Mars");

println(earth.get_name());

println(Planet::PUBLIC_CONST);
println(earth.get_private_const());
