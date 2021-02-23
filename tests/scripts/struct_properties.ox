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
