struct Person {
    pub age: int,
    pub name: str,
}

let mut john: Person = Person { age: 60, name: "John Doe" };

dbg(john.age);
dbg(john.name);

john.age = 40;
john.name = "Jane Doe";

dbg(john.age);
dbg(john.name);

fn clone_person(person: Person) -> Person {
    return Person { age: person.age, name: person.name };
}

let cloned = clone_person(john);

dbg(cloned.age);
dbg(cloned.name);

let make_older: fn(Person) -> Person = fn (person: Person) -> Person {
    let mut person = person;
    person.age += 40;

    return person;
};

john = make_older(john);

dbg(john.age);
dbg(john.name);

struct Dog {
    pub name: str,
    pub fur: Fur,
}

struct Fur {
    pub dense: bool,
}

let d = Dog {
    name: "Markus",
    fur: Fur { dense: true }
};

dbg(d.fur.dense);

fn shave_dog(dog: Dog) {
    dog.fur.dense = false;
}

shave_dog(d);

dbg(d.fur.dense);

fn grow_fur(fur: Fur) {
    fur.dense = true;
}

grow_fur(d.fur);

dbg(d.fur.dense);

fn rename(dog: Dog) {
    dog.name = "Goodboy";
}

rename(d);

dbg(d.name);

fn change_str(mut string: str) {
    string = "Badboy";
}

change_str(d.name);

dbg(d.name);
