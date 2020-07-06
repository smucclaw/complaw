{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- DO NOT EDIT THIS FILE!
-- direct edits will be clobbered.
-- 
-- this file is autogenerated by tangling ex-20200702-safe-post/README.org
-- open the README.org in emacs and hit C-c C-v t to regenerate this file.

module SAFE.Basic where
import Data.Ratio
import Data.Maybe
import Math.LinearEquationSolver

type Percentage = Float
type Money = Float
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
                               , convertibles   :: [Security]  -- this round may cause the conversion of some existing SAFEs, etc
                               , incoming       :: [Security]  -- and we know that some investors have already committed.
                               } deriving (Show, Eq)
data Scenario = LiquidityEvent { liquidityPrice :: Money
                               , common         :: Int
                               , optionsUsed    :: Int
                               , optionsFree    :: Int
                               , convertibles   :: [Security]
                               } deriving (Show, Eq)
estimatedDilution :: [Security] -> Float
estimatedDilution safes =
  sum [ money / cap | SAFE{money_in=money, val_cap=(Just cap)} <- safes ]
dilutionDueTo :: Money -> Security -> Percentage
dilutionDueTo valuationPre safe =
       let effectiveValuation = case (safe.discount, safe.val_cap) of
                         (Nothing, Nothing) -> valuationPre
                         (Nothing, Just _ ) ->     cappedValuation
                         (Just _,  Nothing) ->                     discountedValuation
                         (Just _,  Just _ ) -> min cappedValuation discountedValuation
           cappedValuation     = min (safe.val_cap) (Just valuationPre) // valuationPre
           discountRate        = 1 - safe.discount // 0
           discountedValuation = discountRate * valuationPre
        in safe.money_in / effectiveValuation
sharesPre eqr = eqr.commonPre + eqr.optionsPreOutstanding + eqr.optionsPrePromised + eqr.optionsPreFree
companyCapitalization' eqr = sharesPre eqr + conversionSharesAll' eqr
companyCapitalization  eqr = sharesPre eqr + conversionSharesAll  eqr
conversionSharesAll :: EquityRound -> Int
conversionSharesAll' eqr = ceiling $ conversionDilutions eqr * (fromIntegral (sharesPre eqr) / (1 - conversionDilutions eqr))
conversionSharesAll  eqr = sum $ conversionShares eqr <$> eqr.convertibles
conversionDilutions :: EquityRound -> Float
conversionDilutions eqr =
  sum $ dilutionDueTo (eqr.valuationPre) <$> (eqr.convertibles)
conversionShares :: EquityRound -> Security -> Int
conversionShares eqr safe
  = floor(dilutionDueTo (valuationPre eqr) safe * fromIntegral ( companyCapitalization' eqr ))
totalPost' eqr =
  let cc    = fromIntegral(companyCapitalization eqr)
      vp    =              valuationPre          eqr
      op    =              optionsPost           eqr
      opf   = fromIntegral(optionsPreFree        eqr)
      aim   =              allInvestorMoney      eqr
  in
    floor ( (aim*cc - aim*opf - vp*opf + vp*cc) / (vp - vp*op - aim*op) )
allInvestorMoney :: EquityRound -> Money
allInvestorMoney eqr
  = sum $ money_in <$> eqr.incoming
optionsNewFree' :: EquityRound -> Int
optionsNewFree' eqr
  = floor (optionsPost eqr * fromIntegral(totalPost' eqr)) - optionsPreFree eqr

pricePerShare' :: EquityRound -> Money
pricePerShare' eqr
  = valuationPre eqr / fromIntegral (companyCapitalization eqr + optionsNewFree' eqr)
pricePerShare :: EquityRound -> Money
pricePerShare eqr = fromIntegral(floor(pricePerShare' eqr * 10000)) / 10000

optionsNewFree :: EquityRound -> Int
optionsNewFree eqr = floor000( round(valuationPre eqr / pricePerShare eqr) - companyCapitalization eqr )

floor000 n = n `div` 1000 * 1000

totalPost :: EquityRound -> Int
totalPost eqr = companyCapitalization eqr + allInvestorIssues eqr + optionsNewFree eqr
investorIssue' :: EquityRound -> Security -> Int
investorIssue' eqr investment = floor (investment.money_in / pricePerShare' eqr)
investorIssue  eqr investment = floor (investment.money_in / pricePerShare  eqr)
allInvestorIssues' :: EquityRound -> Int
allInvestorIssues' eqr = sum $ investorIssue' eqr <$> eqr.incoming
allInvestorIssues  eqr = sum $ investorIssue  eqr <$> eqr.incoming
infixl 7 //
(//) = flip fromMaybe
