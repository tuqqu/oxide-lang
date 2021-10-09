// trait with methods
trait Animal {
    // void method
    fn make_noise(self);
    
    // returns value
    fn get_name(self) -> str;
    
    // with params
    fn clone_with_name(self, name: str) -> Animal;
}

// marker trait with no methods
trait LandAnimal {}

// cat
struct Cat {
    pub name: str
}

// impl without trait
impl Cat {
    pub fn accept_cat_return_animal(self, cat: Self) -> Animal {
        dbg("Cat::accept_cat_return_animal");
        return cat;
    }
}

impl Animal for Cat {
    fn make_noise(self) {
        dbg("cat makes noise");
    }
    
    fn get_name(self) -> str {
        return self.name;
    }
    
    fn clone_with_name(self, name: str) -> Animal {
        return Self {
            name: name,
        };
    }
}

impl LandAnimal for Cat {}


// dog

struct Dog {}

impl Animal for Dog {
    fn make_noise(self) {
        dbg("dog makes noise");
    }
    
    fn get_name(self) -> str {
        return "just a dog";
    }
    
    fn clone_with_name(self, name: str) -> Animal {
        return Self {};
    }
}

impl LandAnimal for Dog {}


// whale

struct Whale {
    pub name: str,
}

impl Animal for Whale {
    fn make_noise(self) {
        dbg("whale makes noise");
    }
    
    fn get_name(self) -> str {
        return self.name;
    }
    
    fn clone_with_name(self, name: str) -> Animal {
        return Self {
            name: name
        };
    }
}

// fns for trait objects
fn accept_animal_return_animal(animal: Animal) -> Animal {
    return animal;
}

fn accept_animal_call_methods(a: Animal) {
    a.make_noise();
    let name = a.get_name();
    dbg(name);

    let cloned = a.clone_with_name("new name");
    dbg(cloned);
}

fn accept_land_animal(lanimal: LandAnimal) {
    dbg(lanimal);

    return nil;
}

// tests

let cat = Cat { name: "Thomas" };
let dog = Dog {};
let whale = Whale { name: "White Whale"};

let returned = cat.accept_cat_return_animal(cat);
dbg(returned);

accept_animal_call_methods(cat);
let returned = accept_animal_return_animal(cat);
dbg(returned);

accept_animal_call_methods(dog);
let returned = accept_animal_return_animal(dog);
dbg(returned);

accept_animal_call_methods(whale);
let returned = accept_animal_return_animal(whale);
dbg(returned);

// test marker trait
accept_land_animal(cat);
accept_land_animal(dog);
