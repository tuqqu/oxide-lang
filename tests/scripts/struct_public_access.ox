struct Planet {
    pub age: int,
    name: str,
}

impl Planet {
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
