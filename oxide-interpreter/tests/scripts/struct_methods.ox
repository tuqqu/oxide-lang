struct Person {
    pub age: int,
    pub first_name: str,
    pub last_name: str,
    pub alive: bool,
    pub pet: Dog
}

impl Person {
    pub fn die(self) {
        self.alive = false;
    }

    pub fn get_full_name(self) -> str {
        return self.first_name + " " + self.last_name;
    }
}


struct Dog {
    pub name: str,
}

impl Dog {
    pub fn rename(self, new_name: str) {
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

dbg(john.alive);
dbg(john.get_full_name());

john.pet.rename("Steven");

dbg(john.pet);
