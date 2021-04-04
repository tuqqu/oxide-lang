struct Person {
    pub age: int,
    pub first_name: str,
    pub last_name: str,
    pub alive: bool,
    pub pet: Dog
}

impl Person {
    pub fn die() {
        self.alive = false;
    }

    pub fn get_full_name() -> str {
        return self.first_name + " " + self.last_name;
    }
}


struct Dog {
    pub name: str,
}

impl Dog {
    pub fn rename(new_name: str) {
        self.name = new_name;
    }
}

let dog = Dog {
    name: "Markus",
};

let mut john: Person = Person {
    age: 60,
    first_name: "John",
    last_name: "Doe",
    alive: true,
    pet: dog
 };

john.die();

println(john.alive);
println(john.get_full_name());

john.pet.rename("Steven");

println(john.pet);
