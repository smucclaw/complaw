
module Main where

import Maybe
import List
import Float

main
  | equations
  = [ "x=" ++ show x ]
  where
    x, y free
    equations = 10 <= x
              & x <= 12
              & x == y * 2
              & y == 6


type Percentage = Float
type      Money = Float
data Security = SAFE { owner    :: Entity       -- who purchased this safe
                     , money_in :: Money        -- how much money did the investor put in?
                     , discount :: Maybe Float  -- usually something like 20%
                     , val_cap  :: Maybe Money  -- usually something like US$10,000,000
                     }
              | Equity { owner      :: Entity
                       , money_in   :: Money
                       , shareClass :: String   -- "A" or "B" or "Seed" depending on the Series
                       }
              deriving (Show, Eq)
type Entity = String -- simple type alias, nothing to see here

data EquityRound = EquityRound { valuationPre   :: Money       -- what pre-money valuation was negotiated and agreed with new investors?
                               , new_money_in   :: Money       -- how much fresh money is coming in?
                               , commonPre      :: Int         -- how many ordinary shares did the company issue immediately prior to the round?
                               , optionsPreOutstanding :: Int  -- what options pool was previously allocated and issued?
                               , optionsPrePromised    :: Int  -- what options pool was previously allocated and promised, but not yet issued?
                               , optionsPreFree        :: Int  -- what options pool was previously allocated but not spoken for?
                               , optionsPost    :: Float       -- what pool is being set aside in this round, as a percentage of post?
                               , convertibles_eqr   :: [Security]  -- this round may cause the conversion of some existing SAFEs, etc
                               , incoming       :: [Security]  -- and we know that some investors have already committed.
                               } deriving (Show, Eq)
data Scenario = LiquidityEvent { liquidityPrice :: Money
                               , common         :: Int
                               , optionsUsed    :: Int
                               , optionsFree    :: Int
                               , convertibles_scenario   :: [Security]
                               } deriving (Show, Eq)
estimatedDilution :: [Security] -> Float
estimatedDilution safes =
  sum [ money / cap
      | SAFE{money_in=money, val_cap=(Just cap)} <- safes ]

safe_a, safe_b :: Security
safe_a = SAFE { owner="Investor A", money_in=200000, discount=Nothing, val_cap=(Just 4000000) }
safe_b = SAFE         "Investor B"           800000           Nothing          (Just 8000000)

series_a :: EquityRound
series_a = EquityRound { valuationPre = 15000000
                       , new_money_in =  5000000
                       , commonPre    =  9250000
                       , optionsPreOutstanding = 300000
                       , optionsPrePromised    = 350000
                       , optionsPreFree = 100000
                       , optionsPost  = 10 / 100
                       , convertibles_eqr = [safe_a, safe_b]
                       , incoming     = [seriesA_c, seriesA_b, seriesA_other]
                       }

seriesA_c, seriesA_b, seriesA_other :: Security
seriesA_c     = Equity { owner="Investor C", money_in=4000000,   shareClass="A" }
seriesA_b     = Equity { owner="Investor B", money_in=499998.97, shareClass="A" }
seriesA_other = Equity "Other New Investors"          500001.19             "A"

exit10 :: Scenario
exit10 = LiquidityEvent { liquidityPrice = 10000000
                        , common         =  9250000
                        , optionsUsed    =   300000
                        , optionsFree    =   450000
                        , convertibles_scenario   = [safe_a, safe_b]
                        }

dilutionDueTo :: Money -> Security -> Percentage
dilutionDueTo valuationPre safe =
       let effectiveValuation = case (discount safe, val_cap safe) of
                         (Nothing, Nothing) -> valuationPre
                         (Nothing, Just _ ) ->     cappedValuation
                         (Just _,  Nothing) ->                     discountedValuation
                         (Just _,  Just _ ) -> min cappedValuation discountedValuation
           cappedValuation     = min (val_cap safe) (Just valuationPre) // valuationPre
           discountRate        = 1 - discount safe // 0
           discountedValuation = discountRate * valuationPre
        in money_in safe / effectiveValuation

conversion eqr
  | sPre == i2f( commonPre eqr + optionsPreOutstanding eqr + optionsPrePromised eqr + optionsPreFree eqr )
  & valPre == valuationPre eqr
  & cc    =:= sPre + csall
  & csall =:= (dilutionDueTo valPre safe_a * cc + dilutionDueTo valPre safe_b * cc)
  & pps   =:= valuationPre eqr / (cc + onf)
  & onf   =:= optionsPost eqr * tp - i2f(optionsPreFree eqr)
  & tp    =:= cc + aii + onf
  & aii   =:= allInvestorMoney eqr / pps
  = [ "totalPost = " ++ show tp ]
  where sPre, valPre, cc, pps, onf, tp, aii, csall free
        convShares safe = dilutionDueTo (valuationPre eqr) safe * cc

sharesPre eqr = sum $ [commonPre eqr, optionsPreOutstanding eqr, optionsPrePromised eqr, optionsPreFree eqr]

companyCapitalization' eqr = sharesPre eqr + conversionSharesAll' eqr
companyCapitalization  eqr = sharesPre eqr + conversionSharesAll  eqr

conversionSharesAll :: EquityRound -> Int
conversionSharesAll' eqr = round $ conversionDilutions eqr * (i2f (sharesPre eqr) / (1 - conversionDilutions eqr))
conversionSharesAll  eqr = sum $ fmap (conversionShares eqr) (convertibles_eqr eqr)

conversionDilutions :: EquityRound -> Float
conversionDilutions eqr =
  sum $ fmap (dilutionDueTo (valuationPre eqr)) (convertibles_eqr eqr)

conversionShares :: EquityRound -> Security -> Int
conversionShares eqr safe
  = truncate(dilutionDueTo (valuationPre eqr) safe * i2f ( companyCapitalization' eqr ))

totalPost' eqr =
  let cc    = i2f(companyCapitalization eqr)
      vp    =              valuationPre          eqr
      op    =              optionsPost           eqr
      opf   = i2f(optionsPreFree        eqr)
      aim   =              allInvestorMoney      eqr
  in
    truncate ( (aim*cc - aim*opf - vp*opf + vp*cc) / (vp - vp*op - aim*op) )

allInvestorMoney :: EquityRound -> Money
allInvestorMoney eqr
  = sum $ fmap money_in (incoming eqr)

optionsNewFree' :: EquityRound -> Int
optionsNewFree' eqr
  = truncate (optionsPost eqr * i2f(totalPost' eqr)) - optionsPreFree eqr

pricePerShare' :: EquityRound -> Money
pricePerShare' eqr
  = valuationPre eqr / i2f (companyCapitalization eqr + optionsNewFree' eqr)

pricePerShare :: EquityRound -> Money
pricePerShare eqr = i2f(truncate(pricePerShare' eqr * 10000)) / 10000

optionsNewFree :: EquityRound -> Int
optionsNewFree eqr = truncate000( round(valuationPre eqr / pricePerShare eqr) - companyCapitalization eqr )

truncate000 n = n `div` 1000 * 1000

totalPost :: EquityRound -> Int
totalPost eqr = companyCapitalization eqr + allInvestorIssues eqr + optionsNewFree eqr

investorIssue' :: EquityRound -> Security -> Int
investorIssue' eqr investment = truncate (money_in investment / pricePerShare' eqr)

investorIssue  eqr investment = truncate (money_in investment / pricePerShare  eqr)

allInvestorIssues' :: EquityRound -> Int
allInvestorIssues' eqr = sum $ fmap (investorIssue' eqr) (incoming eqr)
allInvestorIssues  eqr = sum $ fmap (investorIssue  eqr) (incoming eqr)

infixl 7 //
(//) = flip fromMaybe


i2f = fromInt
