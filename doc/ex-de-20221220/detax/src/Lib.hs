{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State ( get, gets, State, StateT, evalState, runStateT )
import Control.Monad.State (liftIO)
import Control.Monad (forM_)
import Text.Megaparsec
    ( choice, many, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar, hspace )
import Data.List ( sort, isPrefixOf, nub )
import Text.PrettyPrint.Boxes
    ( emptyBox, hsep, nullBox, render, vcat, Box )
import qualified Text.PrettyPrint.Boxes as BX

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance Semigroup Box where
  (<>) = (BX.<>)

instance Monoid Box where
  mempty = nullBox



-- | basic mathematical algebra calculator expression language
-- augmented with if\/then\/else construct and variable assignment

data MathLang a
  = MathLang a :+: MathLang a              -- ^ addition
  | MathLang a :-: MathLang a              -- ^ subtraction
  | MathLang a :*: MathLang a              -- ^ multiplication
  | MathLang a :/: MathLang a              -- ^ division
  | Parens (MathLang a)                    -- ^ parentheses
  | MathVal a                              -- ^ terminal value
  | MathVar String                         -- ^ mathematical variable name
  | MathITE (Pred a) (MathLang a) (MathLang a) -- ^ if then else, mathematicals
  deriving (Eq, Show)

-- | conditional predicates
data Pred a
  = PredEqB (Pred a) (Pred a)              -- ^ boolean equality test
  | PredNot (Pred a)                       -- ^ boolean not
  | PredEqM (MathLang a) (MathLang a)      -- ^ mathematical equality
  | PredGte (MathLang a) (MathLang a)      -- ^ >=
  | PredGt  (MathLang a) (MathLang a)      -- ^ >
  | PredLte (MathLang a) (MathLang a)      -- ^ <=
  | PredLt  (MathLang a) (MathLang a)      -- ^ <
  | PredVar String                         -- ^ boolean variable name
  | PredITE (Pred a) (Pred a) (Pred a)     -- ^ if then else, booleans
  deriving (Eq, Show)

-- | variables
data Var a
  = VarMath (MathLang a)                   -- ^ variable assignment
  | VarPred (Pred     a)                   -- ^ boolean predicate assignment
  deriving (Eq, Show)

type VarTable a = Map.Map String (Var a)

evalMath :: (Fractional a, Ord a) => MathLang a -> State (VarTable a) a
evalMath (x :+: y)   = (+) <$> evalMath x <*> evalMath y
evalMath (x :-: y)   = (-) <$> evalMath x <*> evalMath y
evalMath (x :*: y)   = (*) <$> evalMath x <*> evalMath y
evalMath (x :/: y)   = (/) <$> evalMath x <*> evalMath y
evalMath (Parens x)  = evalMath x
evalMath (MathVal x) = return x
evalMath (MathVar s) = do
  varmath <- gets (Map.! s)
  case varmath of
    (VarMath y) -> evalMath y
    _           -> error $ "variable " <> s <> " is not a math var"
evalMath (MathITE x y z) = do
  ifval <- evalPred x
  evalMath (if ifval then y else z)
  
evalPred :: (Fractional a, Ord a) => Pred a -> State (VarTable a) Bool
evalPred (PredEqB p1 p2) = (==) <$> evalPred p1 <*> evalPred p2
evalPred (PredNot p)     = not  <$> evalPred p
evalPred (PredEqM m1 m2) = (==) <$> evalMath m1 <*> evalMath m2
evalPred (PredGte m1 m2) = (>=) <$> evalMath m1 <*> evalMath m2
evalPred (PredGt  m1 m2) = (> ) <$> evalMath m1 <*> evalMath m2
evalPred (PredLte m1 m2) = (<=) <$> evalMath m1 <*> evalMath m2
evalPred (PredLt  m1 m2) = (< ) <$> evalMath m1 <*> evalMath m2
evalPred (PredVar s) = do
  varpred <- gets (Map.! s)
  case varpred of
    (VarPred y) -> evalPred y
    _           -> error $ "variable " <> s <> " is not a Boolean predicate"
evalPred (PredITE x y z) = do
  ifval <- evalPred x
  evalPred (if ifval then y else z)

type Parser = Parsec () String

-- sse the Expr combinators lib from parsec to do this -- we can't deal with precedence correctly here
pMathLang :: (Fractional a) => Parser (MathLang a)
pMathLang =
  many hspace *>
  tryChoice 
  [ MathVal . fromIntegral <$> int
--  , MathVar <$> (string "$" *> some alphaNumChar)
--  , Parens  <$> (string "(" *> hspace *> pMathLang <* hspace <* string ")")
--  , (:+:)   <$> (pMathLang <* many hspace <* string "+") <*> (many hspace *> pMathLang)
--  , (:-:)   <$> (pMathLang <* many hspace <* string "-") <*> (many hspace *> pMathLang)
--  , (:*:)   <$> (pMathLang <* many hspace <* string "*") <*> (many hspace *> pMathLang)
--  , (:/:)   <$> (pMathLang <* many hspace <* string "/") <*> (many hspace *> pMathLang)
--
  ]
  <* many hspace

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . fmap try

toEng :: MathLang a -> String
toEng _ = "[TODO]"

int :: Parser Int
int = read <$> some numberChar

defaultScenario :: Scenario
defaultScenario =
  mkMap [ (i, defaultStream)
        | i <- [ "ordinary income"
               , "extraordinary income"
               , "ordinary expenses"
--               , "special expenses"
--               , "lump sum deductions"
               ]
        , let defaultStream :: IncomeStreams
              defaultStream =
                mkMap [ (ic,0)
                      | ic <- [ "Agriculture"
                              , "Trade"
                              , "Independent"
                              , "Employment"
                              , "Exempt Capital"
                              , "Capital"
                              , "Rents"
                              , "Other"
                              ] ]
        ]
  where mkMap = Map.fromList

runTests :: IO ()
runTests = do
  let symtab = Map.fromList [("foo", VarMath (MathVal (5 :: Float)))]
  let test1 = evalState (evalMath (MathITE
                                   (PredGt (MathVal 1) (MathVal 2))
                                   (MathVar "foo")
                                   (MathVal 6))) symtab :: Float
  print test1
  -- [TODO] write a simple parser to set up the scenario
  let scenario1 =
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure   72150) "Rents"
          . Map.update (const $ pure   30000) "Agriculture"
          . Map.update (const $ pure     100) "Exempt Capital"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure  270000) "Agriculture"
          . Map.update (const $ pure     100) "Exempt Capital"
        ) $

        flip Map.update "ordinary expenses"
        (pure
          . Map.update (const $ pure    2150) "Rents"
          . Map.update (const $ pure    6000) "Independent"
          . Map.update (const $ pure  100000) "Other"
        )
        defaultScenario
  let scenario2 =
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure   72150) "Rents"
          . Map.update (const $ pure   30000) "Agriculture"
          . Map.update (const $ pure     100) "Exempt Capital"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure  270000) "Agriculture"
          . Map.update (const $ pure     100) "Exempt Capital"
        ) $

        flip Map.update "ordinary expenses"
        (pure
          . Map.update (const $ pure    2150) "Rents"
          . Map.update (const $ pure    6000) "Independent"
        )
        defaultScenario

  let testcase1 =
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure   72150) "Rents"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure   25000) "Rents"
        ) $

        defaultScenario

  forM_ (zip [1 :: Int ..] [scenario1, scenario2, testcase1]) $ \(n,sc) -> do
    putStrLn $ "* run " <> show n
    runStateT section_34_1 sc
  return ()
  
--  let parsed = runParser pMathLang "" "( 5 + 3 * 2 )"
--  print parsed

type Scenario      = Map.Map String         IncomeStreams
type IncomeStreams = Map.Map IncomeCategory Float

type IncomeCategory = String

type NetIncome     = Int
type TaxableIncome = Int

-- | render a Scenario as a table -- something like this:

asTable :: Scenario -> Box
asTable sc =
  hsep 2 BX.left (
  -- row headers at left
  vcat BX.left (emptyBox 1 1 : (BX.text <$> sort (Map.keys (head (Map.elems sc)))))
    : [ vcat BX.left (
          -- column headers at top
          BX.text streamName
            : [ BX.text (show (round numval :: Int))
              | numval <- Map.elems streamVal -- is auto sorted by incomeCategory i think
              ] )
      | (streamName, streamVal) <- Map.toAscList sc
      ]
  )

-- | render a single column as a mini table
asColumn :: String -> IncomeStreams -> Box
asColumn str istream = asTable (Map.singleton str istream)

-- | "natural language" friendly way of phrasing a transformation that changes some elements around
data Replace k a = Replace { elems_  :: [k] , with_ :: [Map.Map k a -> Map.Map k a] }

-- | run the generic replacement
runReplace :: Ord k => Map.Map k a -> Replace k a -> Map.Map k a
runReplace m (Replace ks fs) =
  if all (`Map.member` m) ks
  then Map.unions $ (fs <*> [m]) ++ [foldl (flip Map.delete) m ks]
  else m

-- | syntactic sugar for setting up a Replace transformation
(~->) :: [k] -> [Map.Map k a -> Map.Map k a] -> Replace k a
(~->) k ka = Replace { elems_ = k, with_ = ka }

-- | section 2(3) EStG
section_2 :: StateT Scenario IO Scenario
section_2 = do
  initialScenario <- get
  liftIO $ putStrLn "* section_2_3"
  let transformations =
        [ []                                            ~-> []
        , ["ordinary income", "ordinary expenses"]      ~-> [preNetIncome]       -- 2_3_2
        , ["pre-net income"]                            ~-> [offsetLosses_2_3_3] -- 2_3_3
        , []                                            ~-> [squashCats]
      --  , marital
        ]
      steps = scanl runReplace initialScenario transformations
  return $ last steps

-- | run through a specific set of transformations defined in section 34_1.
-- 
-- A particular transformation runs only if all its LHS columns are found in the scenario table,
-- in which case those columns are replaced by the output of the transformer.

section_34_1 :: StateT Scenario IO Scenario
section_34_1 = do
  initialScenario <- get
  liftIO $ putStrLn "* section_34_1"

  let transformations =
        [ []                                            ~-> []
        , ["ordinary income", "ordinary expenses"]      ~-> [preNetIncome]
        , ["pre-net income"]                            ~-> [offsetLosses]
        , []                                            ~-> [squashCats]
        , []                                            ~-> [extraordinary]
        , []                                            ~-> [sentence3]
        , ["1 revised RTI taxation due to sentence 3"]  ~-> [sentence3_b]
        , []                                            ~-> [totalPayable]
        ]
      steps = scanl runReplace initialScenario transformations

  _ <- liftIO $ sequence [ putStrLn ("** step " <> show n) >> putStrLn (asExample step)
                         | (n, step) <- zip [1::Int ..] steps ]

  return $ last steps

preNetIncome :: Scenario -> Scenario
preNetIncome sc =
  Map.singleton "pre-net income" $
  mapAp (-) (sc Map.! "ordinary income") (sc Map.! "ordinary expenses")

-- | loss offsets, based on section 34.
-- losses from one income category can be used to offset earnings in another.
-- the offsetting is done on a per-category basis, against the single "pre-net income" column, pro rata
--
offsetLosses :: Scenario -> Scenario
offsetLosses sc =
  let orig = sc Map.! "pre-net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum $ Map.elems negatives
      totalPos = sum $ Map.elems positives
  in
    Map.singleton "remaining taxable income" $
    (\x -> if x < 0
           then 0 -- [TODO] correctly handle a situation where the negatives exceed the positives
           else x + totalNeg * (x / totalPos))
    <$> orig 


-- | loss offsets, based on section 2(3) para 3 & 4
-- the reduction is done on a per-category basis, against the single "pre-net income" column, pro rata
--
offsetLosses_2_3_3 :: Scenario -> Scenario
offsetLosses_2_3_3 sc =
  let orig = sc Map.! "pre-net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum $ Map.elems negatives
      totalPos = sum $ Map.elems positives
      maxReduction = if   totalPos > 100000
                     then totalNeg / 2
                     else totalNeg
  in
    Map.singleton "remaining taxable income" 
    (offsetLosses_2_3_4 (maxReduction / totalPos) <$> orig)

-- | based on section 2(3) para 4
-- 
-- 4. The reduction is to be made in proportion to
--    1. the positive totals of income
--       1. from different types of income
--    2. to the total of positive income.

offsetLosses_2_3_4 :: Float -> Float -> Float
offsetLosses_2_3_4 reduction x =
  if x < 0
  then 0 -- [TODO] correctly handle a situation where the negatives exceed the positives
  else x + (x * reduction)


mapAp :: Ord k => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a
mapAp = Map.unionWith

-- | extraordinary income is taxed
extraordinary :: Scenario -> Scenario
extraordinary sc =
  let ordinary = sc Map.! "remaining taxable income"
      extraI   = sc Map.! "extraordinary income"
      taxFor   = mapOnly (>0) (progDirect 2023)
      ordtax   = taxFor ordinary
      totalI   = mapAp (+) ordinary extraI
      rti5     = mapAp (+) ordinary ((/5) <$> extraI)
      rti5tax  = taxFor rti5
      delta    = abs <$> mapAp (-) ordtax rti5tax
      rti5tax5 = (5*) <$> delta
      
  in Map.fromList [("1 RTI taxation",           ordtax)
                  ,("2 RTI plus one fifth",     rti5)
                  ,("3 tax on RTI+.2",          rti5tax)
                  ,("4 difference",             delta)
                  ,("5 extraordinary taxation", rti5tax5)
                  ,("total taxable income",     totalI)
                  ]
  
-- | sentence 3
sentence3 :: Scenario -> Scenario
sentence3 sc =
  let ordinary = sc Map.! "remaining taxable income"
      extraI   = sc Map.! "extraordinary income"
      negRTI   = (\o -> if o < 0 then 1 else 0) <$> ordinary
      fiveOnFifth = mapAp (\o e -> if o < 0 && e + o > 0
                                   then 5 * progDirect 2023 ( (o + e) / 5 )
                                   else 0)
                    ordinary extraI
  in if any (>0) (Map.elems negRTI)
     then Map.fromList [("0 RTI is negative",                        negRTI)
                       ,("1 revised RTI taxation due to sentence 3", fiveOnFifth)
                       ]
     else Map.empty

sentence3_b :: Scenario -> Scenario
sentence3_b sc =
  Map.fromList [("1 RTI taxation", sc Map.! "1 revised RTI taxation due to sentence 3")]


totalPayable :: Scenario -> Scenario
totalPayable sc =
  let rtiTax   = sc Map.! "1 RTI taxation"
      extraTax = sc Map.! "5 extraordinary taxation"
  in Map.fromList [("6 total tax payable", mapAp (+) rtiTax extraTax)]


-- | squash all non-exempt income categories together
squashCats :: Scenario -> Scenario
squashCats sc =
  (\istream -> runReplace istream (categoryKeys ~-> [squashCats'])) <$> sc
  where
    categoryKeys = nub (concatMap Map.keys (Map.elems sc))
    squashCats' :: IncomeStreams -> IncomeStreams
    squashCats' ns =
      Map.singleton "total" $ sum $ Map.elems $ Map.filterWithKey (\k _ -> not ("Exempt " `isPrefixOf` k)) ns

-- | fmap only those elements that qualify, leaving the others alone
mapOnly :: Functor t => (a -> Bool) -> (a -> a) -> t a -> t a
mapOnly test transform items = (\i -> if test i then transform i else i) <$> items

-- | when we start paying attention to marital status, we will want to know about different tax classes 
data TaxClass
  = TC1 -- ^ Applies to single, widowed, divorced, permanently separated couples and married couples with one spouse living abroad.
  | TC2 -- ^ Applies to single parents who can claim the relief for single parents
  | TC3 -- ^ - Applies to married persons whose spouse is either not employed or belongs to tax class V as an employee.
        --   - Applies to married persons whose spouse lives in an EU country.
        --   - Applies to widowers in the first year after the deceased spouse's death.
  | TC4 -- ^ - Applies to married spouses who live together and both are subject to unlimited tax liability.
        --   - The tax category combination IV/IV is particularly worthwhile if both spouses earn approximately the same amount.
        --   - In the case of the tax category combination IV with factor, tax-exempt amounts are taken into account in the wage tax calculation from the outset. As a result, the difference between the amount of income tax paid and the actual tax liability at the end of the year is lower.
  | TC5 -- ^ Applies to married persons, if the other spouse is in tax class III (provided the spouses live together).
  | TC6 -- ^ Applies to single and married persons with another employment, if no employment tax card has been issued by the tax office.
  deriving (Ord, Eq, Show)

data Marital = Single | Married
  deriving (Eq, Show)

-- | rate table from the "Tariff history" PDF downloaded from https://www.bmf-steuerrechner.de/
--
-- progressive individual tax rate table, by year.
-- note that these rates do not include:
-- - solidarity tax 5.5% of the normal rate payable; single taxpayers < 62127 / couples < 124255 exempt
-- - church tax 8 -- 9%
-- - business income
--   - corporate tax 15%, reduced for part
--   - municipal business tax of 14 to 17%
--   - municipal trade tax of 7 -- 17%

rateTable :: (Fractional a) => Int -> [(Ordering, a, a -> a)]
-- | ratetable for 2022 -- use only with progStack
rateTable 2022 = [(LT,   9409, const  0)
                 ,(GT,   9408, const 14)
                 ,(GT,  57051, const 42)
                 ,(GT, 270500, const 45)
                 ]
-- | ratetable for 2023 -- use only with progDirect
rateTable 2023 = [(LT,  10909, const  0)
                 ,(GT,  10908, \zvE -> let y = (zvE - 10908) / 10000
                                       in (979.18 * y + 1400) * y)
                 ,(GT,  15999, \zvE -> let y = (zvE - 15999) / 10000
                                       in (192.59 * y + 2397) * y + 966.53)
                 ,(GT,  62810, \zvE -> 0.42 * zvE - 9972.98)
                 ,(GT, 277825, \zvE -> 0.45 * zvE - 18307.73)
                 ]
rateTable _    = rateTable 2023

-- | Direct computation of progressive tax, without recursing to lower tiers of the stack.
-- this is for countries which just give a direct formula that already takes into account
-- the lower tiers of the stack.
progDirect :: (Ord a, Fractional a) => Int -> a -> a
progDirect year income =
  let rt = reverse $ rateTable year
  in go rt income
  where
    go ((o,n,f):rts) income'
      | income' `compare` n == o = f income'
      | otherwise                = go rts income'
    go [] _ = 0

-- | Stacked computation of progressive tax, by recursively adding lower tiers.
-- this is suitable for countries that announce their progressive tiers in terms of
-- "for the nth dollar you make, pay X in taxes on it".
-- This is not to be actually used, it's just here because we wrote it before progDirect.
progStack :: (Ord a, Fractional a) => Int -> a -> a
progStack year income =
  let rt = reverse $ rateTable year
  in progRate rt income
  where
    progRate ((o,n,r):rts) income'
      | income' `compare` n == o = (r income')/100 * (income' - n) + progRate rts n
      | otherwise                = progRate rts income'
    progRate [] _ = 0

-- | for org-mode purposes
asExample :: Scenario -> String
asExample sc = "\n#+begin_example\n" <> render (asTable sc) <> "#+end_example\n"

-- | what is the effective tax rate? against `progStack`
effectiveRateStacked :: (Ord a, Fractional a) => Int -> a -> a
effectiveRateStacked year income = progStack year income / income * 100

-- | what is the effective tax rate? against `progDirect`
effectiveRateDirect :: (Ord a, Fractional a) => Int -> a -> a
effectiveRateDirect year income = progDirect year income / income * 100

-- | the solidarity surcharge is 5.5% /of/ the rest of the taxes
solidaritySurcharge :: (Fractional a, Ord a) => Int -> a -> a
solidaritySurcharge year income = progDirect year income / income * 0.055 * income

{- Also:
Profits from the sale of private real estate that has been held for more than 10 years, or from the sale of other assets that have been held for more than 12 months is exempt from tax. For shorter holding periods the general tax rates apply.
Sale of a shares when the percentage of the investment is less than 1% is subject to a flat 25% tax. On the other hand, when the percentage of the holding is in excess of 1%, tax is payable on 60% of the profit at normal rates.
-}

