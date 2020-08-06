"use strict";
function nextAvailableDates(date, asOf, limit) {
    return legalDates(asOf)
        .filter(x => x >= date)
        .splice(0, limit);
}
function legalDates(asOf) {
    return fullMoonDates(asOf)
        .flatMap(x => [x, addDays(x, 1)]);
}
function fullMoonDates(asOf) {
    return getDatesFromURL("https://www.almanac.com/astronomy/moon/full/");
}
function getDatesFromURL(_) {
    return (["2020-01-10",
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
        "2020-12-29"].map(x => new Date(x)));
}
console.log("the next available dates are: " + JSON.stringify(nextAvailableDates(new Date("2020-08-06"), new Date("2020-08-06"), 4)));
var Deontic;
(function (Deontic) {
    Deontic["must"] = "MUST";
    Deontic["may"] = "MAY";
    Deontic["shant"] = "SHANT";
})(Deontic || (Deontic = {}));
;
function rule1(world, party, action, actionSpec, exemption) {
    if (action === "trade" && isPotato(actionSpec)) {
        if (isLegalPotato(actionSpec, world.legalDates) ||
            (exemption != undefined && exemption.from == "DirectorOfAgriculture")) {
            return Deontic.may;
        }
        return Deontic.shant;
    }
}
function isPotato(item) {
    return item.hasOwnProperty("species") && item.species === "Tuberosum solanum";
}
function isLegalPotato(item, legalDates) {
    return item.hasOwnProperty("wasPlanted") && legalDates.includes(item.wasPlanted);
}
const potato1 = { species: "Solanum tuberosum", wasPlanted: new Date("2020-01-10") };
const potato2 = { species: "Solanum tuberosum", wasPlanted: new Date("2020-01-11") };
const potato3 = { species: "Solanum tuberosum", wasPlanted: new Date("2020-01-12") };
console.log(`potato1: ` + rule1({ legalDates: legalDates(new Date("2020-08-06")) }, "_", "trade", potato1, undefined));
console.log(`potato2: ` + rule1({ legalDates: legalDates(new Date("2020-08-06")) }, "_", "trade", potato2, undefined));
function addDays(date, days) {
    var result = new Date(date);
    result.setDate(result.getDate() + days);
    return result;
}
