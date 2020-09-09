import { Engine } from 'json-rules-engine'

let engine = new Engine()

engine.addRule({
    conditions: { all: [{ fact: 'thisYear',  operator: 'greaterThan', value: 0 },
                        { fact: 'birthDate', operator: 'greaterThan', value: 0 },
                       ] },
    event: { type: 'age-computation' },
    priority: 20, // IMPORTANT!  Set a higher priority for the age-computation, so it runs first
    onSuccess: function (event, almanac) {
        almanac.addRuntimeFact('age', almanac.factValue('thisYear') - almanac.factValue('birthDate'))
    },
})
engine.addRule({
    conditions: { all: [{ fact: 'age', operator: 'greaterThanInclusive', value: 21 }] },
    event: { type: 'majority-computation' },
    priority: 10, // IMPORTANT!  Set a higher priority for the MajorityRule, so it runs first
    onSuccess: function (event, almanac) {
        almanac.addRuntimeFact('isAdult', true)
    },
    onFailure: function (event, almanac) {
        almanac.addRuntimeFact('isNotAdult', false)
    }
})
engine.addRule({
    conditions: { all: [{ fact: 'age', operator: 'lessThan', value: 4 }] },
    event: { type: 'minority-computation' },
    priority: 10, // IMPORTANT!  Set a higher priority for the MajorityRule, so it runs first
    onSuccess: function (event, almanac) {
        almanac.addRuntimeFact('isBaby', true)
    },
    onFailure: function (event, almanac) {
        almanac.addRuntimeFact('isNotBaby', false)
    }
})
engine.addRule({
    conditions: {
        any: [
            { all: [{ fact: 'food', operator: 'equal', value: "ribeye" },
                    { fact: 'isAdult', operator: 'equal', value: true }]},
            { all: [{ fact: 'food', operator: 'equal', value: "mashedPotatoes" },
                    { fact: 'isBaby', operator: 'equal', value: true }]},
            { all: [{ fact: 'isAdult', operator: 'equal', value: true }]}
        ]
    },
    event: {  // define the event to fire when the conditions evaluate truthy
        type: 'permitted',
        params: {
            message: 'permittedAction!'
        }
    }
})

/**
 * Define facts the engine will use to evaluate the conditions above.
 * Facts may also be loaded asynchronously at runtime; see the advanced example below
 */
let factset = [
    { name:"alice", thisYear: 2020, birthDate: 2019, food: "mashedPotatoes" },
    { name:"alice", thisYear: 2020, birthDate: 2019, food: "ribeye" },
    { name:"bob", thisYear: 2020, birthDate: 1970, food: "mashedPotatoes" },
    { name:"bob", thisYear: 2020, birthDate: 1970, food: "ribeye" }
]

for (var facts of factset) {
engine
  .run(facts)
  .then(results => {
    // 'results' is an object containing successful events, and an Almanac instance containing facts
    results.events.map(event => console.log(event.params.message))
  })
}
