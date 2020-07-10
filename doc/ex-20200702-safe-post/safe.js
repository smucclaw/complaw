"use strict";
exports.__esModule = true;
// so, let's solve SAFE conversion in typescript using some random NPM module we found after 2 minutes of research
// https://www.npmjs.com/package/nerdamer
// nerdamer was never meant to run in Node, but this import incantation seems to work
var nerdamer = require("nerdamer/nerdamer.core");
var Solve = require("nerdamer/Solve");
Solve;
var _ = require("lodash");
var safe_a = { owner: "Investor A", money_in: 200000, val_cap: 4000000 };
var safe_b = { owner: "Investor B", money_in: 800000, val_cap: 8000000 };
function effectiveValuation(valuationPre, safe) {
    var discountRate = 1 - (safe.discount == undefined ? 0 : safe.discount);
    var discountedValuation = discountRate * valuationPre;
    var cappedValuation = Math.min(safe.val_cap, valuationPre);
    if (safe.discount == undefined && safe.val_cap == undefined) {
        return valuationPre;
    }
    if (safe.discount == undefined && safe.val_cap != undefined) {
        return cappedValuation;
    }
    if (safe.discount != undefined && safe.val_cap == undefined) {
        return discountedValuation;
    }
    if (safe.discount != undefined && safe.val_cap != undefined) {
        return Math.min(cappedValuation, discountedValuation);
    }
}
function dilutionDueTo(valuationPre, safe) {
    return safe.money_in / effectiveValuation(valuationPre, safe);
}
var seriesA_c = { owner: "Investor C", money_in: 4000000, shareClass: "A" };
var seriesA_b = { owner: "Investor B", money_in: 499998.97, shareClass: "A" };
var seriesA_other = { owner: "Other New Investors", money_in: 500001.19, shareClass: "A" };
var series_a = {
    valuationPre: 15000000,
    new_money_in: 5000000,
    commonPre: 9250000,
    optionsPreOutstanding: 300000,
    optionsPrePromised: 350000,
    optionsPreFree: 100000,
    optionsPost: 10 / 100,
    convertibles: [safe_a, safe_b],
    incoming: [seriesA_c, seriesA_b, seriesA_other]
};
function sharesPre(eqr) {
    return _.sum([eqr.commonPre, eqr.optionsPreOutstanding, eqr.optionsPrePromised, eqr.optionsPreFree]);
}
function conversionDilutions(eqr) {
    return _.sum([safe_a, safe_b].map(function (safe) { return dilutionDueTo(eqr.valuationPre, safe); }));
}
function conversion(eqr) {
    nerdamer.set('SOLUTIONS_AS_OBJECT', true);
    var sol = nerdamer.solveEquations([
        "conversionDilutions   = " + conversionDilutions(eqr),
        "sharesPre             = " + sharesPre(eqr),
        "conversionSharesAll   = conversionDilutions * companyCapitalization",
        "companyCapitalization = sharesPre + conversionSharesAll",
        "allInvestorMoney      = " + _.sum(eqr.incoming.map(function (x) { return x.money_in; })),
        "pricePerShare         = " + eqr.valuationPre + " / (companyCapitalization + optionsNewFree)",
        "optionsNewFree        = " + eqr.optionsPost + " * totalPost - " + eqr.optionsPreFree,
        "totalPost             = companyCapitalization + allInvestorIssues + optionsNewFree",
        "allInvestorIssues     = allInvestorMoney / pricePerShare",
    ]);
    console.log(sol);
}
conversion(series_a);
