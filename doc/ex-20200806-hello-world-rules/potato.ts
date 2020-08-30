function nextAvailableDates (date: Date, asOf: Date, limit: number) : Date[] {
  return legalDates(asOf)
    .filter(x => x >= date)
    .splice(0, limit)
}

function legalDates(asOf: Date) : Date[] { // rule 4
  return fullMoonDates(asOf)
    .flatMap(x => [x, addDays(x,1)]) // \x -> [x, dateAdd x 1]
}

function fullMoonDates(asOf: Date) : Date[] { // rule 5
  return getDatesFromURL("https://www.almanac.com/astronomy/moon/full/")
}

function getDatesFromURL(_:string) : Date[] {
return ([ "2020-01-10",
          "2020-02-08",
          "2020-03-09",
          "2020-04-07",
          "2020-05-06",
          "2020-06-05",
          "2020-07-04",
          "2020-08-02",
          "2020-09-01",
          "2020-10-01",
          "2020-10-30",
          "2020-11-29",
          "2020-12-29" ].map(x=>new Date(x)))
}

console.log("the next available dates are: " + JSON.stringify(nextAvailableDates(new Date("2020-08-06"), new Date("2020-08-06"), 4)));

interface Clause {
  "conditions": Condition[];
  "party": Party[];
  "deontic": Deontic;
  "action": Action;
  "actionSpec": ActionSpec[];
  "temporal": Temporal;
}

type Condition  = string;
type Party      = string;
enum Deontic { must="MUST", may="MAY", shant="SHANT" };
type Action     = string;
type ActionSpec = Farmed;
type Temporal   = string;
interface World { "legalDates" : Date[] }
interface Exemption { "from": string }

interface Farmed { "species": string; "wasPlanted": Date }

function rule1(world : World, party : Party, action: Action, actionSpec: ActionSpec, exemption?: Exemption) : Deontic | undefined {
  if (action === "trade" && isPotato(actionSpec)) {
    if (isLegalPotato(actionSpec, world.legalDates) ||
      (exemption != undefined && exemption.from == "DirectorOfAgriculture")) {
      return Deontic.may
    }
    return Deontic.shant
  }
}
// functional-style method-at-large
function isPotato(item : Farmed) : boolean { // rule 2
  return item.hasOwnProperty("species") && item.species === "Solanum tuberosum"
}

function isLegalPotato(item : Farmed, legalDates : Date[]) : boolean { // rule 3
  return (isPotato(item) && item.hasOwnProperty("wasPlanted") && legalDates.map(x=>x.valueOf()).includes(item.wasPlanted.valueOf()))
}

const potato1 = {species:"Solanum tuberosum", wasPlanted:new Date("2020-01-10")}
const potato2 = {species:"Solanum tuberosum", wasPlanted:new Date("2020-01-11")}
const potato3 = {species:"Solanum tuberosum", wasPlanted:new Date("2020-01-12")}

console.log(`potato1: ` + rule1({legalDates:legalDates(new Date("2020-08-06"))}, "_", "trade", potato1, undefined))
console.log(`potato2: ` + rule1({legalDates:legalDates(new Date("2020-08-06"))}, "_", "trade", potato2, undefined))
console.log(`potato3: ` + rule1({legalDates:legalDates(new Date("2020-08-06"))}, "_", "trade", potato3, undefined))

function addDays(date : Date, days : number) {
  var result = new Date(date);
  result.setDate(result.getDate() + days);
  return result;
}
