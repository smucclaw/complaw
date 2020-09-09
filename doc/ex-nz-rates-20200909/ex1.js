var Person = /** @class */ (function () {
    function Person(name, birthdate) {
        this.name = name;
        this.birthdate = birthdate;
    }
    Person.prototype.isAdult = function (asOf) { return (asOf.getFullYear() - this.birthdate.getFullYear() >= 21); };
    Person.prototype.isBaby = function (asOf) { return (asOf.getFullYear() - this.birthdate.getFullYear() < 4); };
    return Person;
}());
var Action;
(function (Action) {
    Action[Action["eat"] = 0] = "eat";
})(Action || (Action = {}));
var Foods = ["ribeye", "mashedPotatoes"];
var Activity = /** @class */ (function () {
    function Activity(verb, noun) {
        this.verb = verb;
        this.noun = noun;
    }
    Activity.prototype.permitted = function (p) {
        if (this.verb == Action.eat) {
            switch (this.noun) {
                case "ribeye":
                    if (p.isAdult(new Date("2020-01-01"))) {
                        return true;
                    }
                    ;
                    break;
                case "mashedPotatoes":
                    if (p.isBaby(new Date("2020-01-01"))) {
                        return true;
                    }
                    ;
                default: return p.isAdult(new Date("2020-01-01"));
            }
        }
        return false;
    };
    return Activity;
}());
var alice = new Person("Alice", new Date("2019-01-01"));
var bob = new Person("Bob", new Date("1970-01-01"));
for (var _i = 0, _a = ["ribeye", "mashedPotatoes"]; _i < _a.length; _i++) {
    var food = _a[_i];
    for (var _b = 0, _c = [alice, bob]; _b < _c.length; _b++) {
        var person = _c[_b];
        var activity = new Activity(Action.eat, food);
        console.log("is " + person.name + " allowed to eat " + food + "? " + activity.permitted(person));
    }
}
