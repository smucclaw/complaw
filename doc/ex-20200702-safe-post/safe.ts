// so, let's solve SAFE conversion in typescript using some random NPM module we found after 2 minutes of research
// https://www.npmjs.com/package/nerdamer
// nerdamer was never meant to run in Node, but this import incantation seems to work
import * as nerdamer from 'nerdamer/nerdamer.core'
import * as Solve from 'nerdamer/Solve'; Solve;
import * as _ from 'lodash'

interface SAFE {
  owner     : string
  money_in  : number
  discount ?: number
  val_cap  ?: number
}

let safe_a : SAFE = { owner: "Investor A", money_in: 200000, val_cap: 4000000 }
let safe_b : SAFE = { owner: "Investor B", money_in: 800000, val_cap: 8000000 }

function effectiveValuation (valuationPre : number, safe: SAFE) {
  let discountRate        = 1 - (safe.discount == undefined ? 0 : safe.discount)
  let discountedValuation = discountRate * valuationPre
  let cappedValuation     = Math.min(safe.val_cap, valuationPre)
  if (safe.discount == undefined && safe.val_cap == undefined) { return valuationPre }
  if (safe.discount == undefined && safe.val_cap != undefined) { return cappedValuation }
  if (safe.discount != undefined && safe.val_cap == undefined) { return discountedValuation }
  if (safe.discount != undefined && safe.val_cap != undefined) { return Math.min(cappedValuation, discountedValuation) }
}

function dilutionDueTo (valuationPre : number, safe : SAFE) {
  return safe.money_in / effectiveValuation(valuationPre, safe)
}

interface EquityRound {
  valuationPre          : number
  new_money_in          : number
  commonPre             : number
  optionsPreOutstanding : number
  optionsPrePromised    : number
  optionsPreFree        : number
  optionsPost           : number
  convertibles          : SAFE[]
  incoming              : Equity[]
}

interface Equity {
  owner      : string
  money_in   : number
  shareClass : string
}

let seriesA_c     : Equity = { owner:"Investor C",          money_in: 4000000,   shareClass:"A" }
let seriesA_b     : Equity = { owner:"Investor B",          money_in: 499998.97, shareClass:"A" }
let seriesA_other : Equity = { owner:"Other New Investors", money_in: 500001.19, shareClass:"A" }

let series_a : EquityRound = {
  valuationPre            : 15000000
  , new_money_in          :  5000000
  , commonPre             :  9250000
  , optionsPreOutstanding :   300000
  , optionsPrePromised    :   350000
  , optionsPreFree        :   100000
  , optionsPost           : 10 / 100
  , convertibles          : [safe_a, safe_b]
  , incoming              : [seriesA_c, seriesA_b, seriesA_other]
}

function sharesPre (eqr : EquityRound) {
  return _.sum([eqr.commonPre, eqr.optionsPreOutstanding, eqr.optionsPrePromised, eqr.optionsPreFree])
}

function conversionDilutions (eqr : EquityRound) {
  return _.sum([safe_a, safe_b].map(safe => dilutionDueTo(eqr.valuationPre, safe)))
}

function conversion (eqr : EquityRound) {
  nerdamer.set('SOLUTIONS_AS_OBJECT', true);
  var sol = nerdamer.solveEquations([
    `conversionDilutions   = ${conversionDilutions(eqr)}`,
    `sharesPre             = ${sharesPre(eqr)}`,
    `conversionSharesAll   = conversionDilutions * companyCapitalization`,
    `companyCapitalization = sharesPre + conversionSharesAll`,
    `allInvestorMoney      = ` + _.sum(eqr.incoming.map(x => x.money_in)),
    `pricePerShare         = ${eqr.valuationPre} / (companyCapitalization + optionsNewFree)`,
    `optionsNewFree        = ${eqr.optionsPost} * totalPost - ${eqr.optionsPreFree}`,
    `totalPost             = companyCapitalization + allInvestorIssues + optionsNewFree`,
    `allInvestorIssues     = allInvestorMoney / pricePerShare`,
])
  console.log(sol)
}
conversion(series_a)
