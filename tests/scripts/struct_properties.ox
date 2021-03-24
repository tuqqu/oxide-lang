struct Person {
    age: int,
    name: str,
}

let mut john: Person = Person { age: 60, name: "John Doe" };

println(john.age);
println(john.name);

john.age = 40;
john.name = "Jane Doe";

println(john.age);
println(john.name);

fn clone_person(person: Person) -> Person {
    return Person { age: person.age, name: person.name };
}

let cloned = clone_person(john);

println(cloned.age);
println(cloned.name);

let make_older: func = fn (person: Person) -> Person {
    let mut person = person;
    person.age += 40;

    return person;
};

john = make_older(john);

println(john.age);
println(john.name);

struct Dog {
    name: str,
    fur: Fur,
}

struct Fur {
    dense: bool,
}

let d = Dog {
    name: "Markus",
    fur: Fur { dense: true }
};

println(d.fur.dense);

fn shave_dog(dog: Dog) {
    dog.fur.dense = false;
}

shave_dog(d);

println(d.fur.dense);

fn grow_fur(fur: Fur) {
    fur.dense = true;
}

grow_fur(d.fur);

println(d.fur.dense);

fn rename(dog: Dog) {
    dog.name = "Goodboy";
}

rename(d);

println(d.name);

fn change_str(mut string: str) {
    string = "Badboy";
}

change_str(d.name);

println(d.name);
