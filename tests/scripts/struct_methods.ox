struct Person {
    age: int,
    first_name: str,
    last_name: str,
    alive: bool,
    pet: Dog
}

impl Person {
    fn die() {
        self.alive = false;
    }

    fn get_full_name() -> str {
        return self.first_name + " " + self.last_name;
    }
}


struct Dog {
    name: str,
}

impl Dog {
    fn rename(new_name: str) {
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
