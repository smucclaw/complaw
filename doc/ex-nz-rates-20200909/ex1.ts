class Person {
    constructor( readonly name: String, readonly birthdate: Date ) { }
    isAdult(asOf:Date) { return (asOf.getFullYear() - this.birthdate.getFullYear() >= 21) }
    isBaby (asOf:Date) { return (asOf.getFullYear() - this.birthdate.getFullYear() < 4)  }
}
enum Action { "eat" }
let Foods = ["ribeye", "mashedPotatoes"]
class Activity {
    constructor( readonly verb: Action, readonly noun: String ) { }
    permitted(p:Person): Boolean {
        if (this.verb == Action.eat) {
            switch (this.noun) {
                case "ribeye":         if    (p.isAdult(new Date("2020-01-01"))) { return true }; break;
                case "mashedPotatoes": if    (p.isBaby (new Date("2020-01-01"))) { return true };
                default:               return p.isAdult(new Date("2020-01-01"));
            }
        }
        return false;
    }
}
let alice = new Person ("Alice", new Date("2019-01-01") )
let bob   = new Person ("Bob",   new Date("1970-01-01") )
for (var food of ["ribeye","mashedPotatoes"]) {
    for (var person of [alice,bob]) {
        const activity = new Activity(Action.eat, food);
        console.log(`is ${person.name} allowed to eat ${food}? ${activity.permitted(person)}`);
    }
}
