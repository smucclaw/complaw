{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State ( gets, State, StateT, evalState, runStateT, modify )
import Control.Monad.State (liftIO)
import Control.Monad (forM_, when, unless)
import Text.Megaparsec ( choice, many, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar, hspace )
import Data.List ( sort, isPrefixOf, nub )
import Text.PrettyPrint.Boxes
    ( emptyBox, hsep, nullBox, render, vcat, Box )
import qualified Text.PrettyPrint.Boxes as BX
import Control.Monad.Combinators.Expr
import Data.Ord
import qualified Data.Tree as DT

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

-- | sample input whose semantics can be parsed out of natural4
--
sampleInput1 = ""




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
               , "special expenses"
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
  -- scenario 1: income exceeds expenses thanks to extraordinary; taxable income is > 100000; some negative income in certain categories
  let scenario1a = ("1a",) $
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

  let scenario1b = ("1b",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure   72150) "Rents"
          . Map.update (const $ pure   20000) "Agriculture"
          . Map.update (const $ pure     100) "Exempt Capital"
        ) $

        flip Map.update "ordinary expenses"
        (pure
          . Map.update (const $ pure    2150) "Rents"
          . Map.update (const $ pure    6000) "Independent"
          . Map.update (const $ pure   60000) "Other"
        )
        defaultScenario

  let scenario2 = ("2",) $
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

  let testcase1 = ("test case 1",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure $ 72150 - 25000) "Rents"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure   25000) "Rents"
        ) $

        defaultScenario

  let testcase2 = ("test case 2",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure $ 5350)  "Trade"
--          . Map.update (const $ pure $ 66666) "Employment"
        ) $

        flip Map.update "ordinary expenses"
        (pure
          . Map.update (const $ pure $ 45000) "Rents"
        ) $

        flip Map.update "special expenses"
        (pure
          . Map.update (const $ pure $ 3200) "Rents"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure  225000) "Capital"
        ) $

        defaultScenario

  let testcase3 = ("test case 3",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure $ 5350)  "Trade"
--          . Map.update (const $ pure $ 66666) "Employment"
        ) $

        flip Map.update "ordinary expenses"
        (pure
          . Map.update (const $ pure $ 45000) "Rents"
        ) $

        flip Map.update "special expenses"
        (pure
          . Map.update (const $ pure $ 3200) "Rents"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure  225000) "Capital"
        ) $

        defaultScenario

  let testcase3_fired = ("test case 3 - fired",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure $ 22000) "Employment"
        ) $

        flip Map.update "extraordinary income"
        (pure
          . Map.update (const $ pure  130000) "Employment"
        ) $

        defaultScenario

  let testcase3_unfired = ("test case 3 - unfired",) $
        flip Map.update "ordinary income"
        (pure
          . Map.update (const $ pure $ 22000) "Employment"
        ) $

        defaultScenario

  forM_ [scenario1a, scenario1b, scenario2, testcase1, testcase2
        , testcase3, testcase3_fired, testcase3_unfired] $ \(sctitle,sc) -> do
    putStrLn $ "* running scenario: " <> sctitle

    (result_2_3, expl_2_3) <- runExplainIO $ section_2_3  sc
    unless (null expl_2_3) $ putStrLn ("** explaining section_2_3: " <> sctitle) >> printExplanation expl_2_3

    putStrLn $ "** executing section_34_1: " <> sctitle
    (result_341, expl_341) <- runExplainIO $ section_34_1 sc
    unless (null expl_341) $ putStrLn "** explaining section_34_1:" >> printExplanation expl_341

  putStrLn "* which tax method shall we use to deal with extraordinary income in test case 3?"
  (effMethod, expl) <- runExplainIO $ chooseEffectiveEOTaxMethod testcase3_fired testcase3_unfired
  putStrLn $ "* we choose " ++ show effMethod
  printExplanation expl

  return ()

-- | choose a tax computation method
-- > extraordinary income may only be taxed at a reduced rate if it
-- > leads to an aggregation of income for the tax year in question.

-- the "leads to" is confusing above: there is no implication; it is a biconditional.
--
--          | Aggregation of Income (IN)   |     Tax Computation Method (OUT)
--          |---------------------------------------------------------------
--          |             True             |             One-Fifths
--          |            False             |               Normal

-- > According to a 1997 ruling by the Federal Court of Finance (BFH,
-- > Bundesfinanzhof), extraordinary income may only be taxed at a
-- > reduced rate if it leads to an aggregation of income for the tax
-- > year in question.
-- > 
-- > For this purpose, both your actual total annual income (including
-- > severance pay and other income earned after termination) and the
-- > income you would have earned without termination of employment are
-- > compared (you can base this on your income from the previous year).
-- > 
-- > If your actual income is higher than the income you would have
-- > earned, there is an aggregation of income and the one-fifth method
-- > [the method of computing tax on extraordinary income used earlier]
-- > can be applied.

data EOTaxMethod = EONormal | EOFifth
  deriving (Eq, Show)

chooseEffectiveEOTaxMethod :: (String,Scenario) -- ^ actual
                           -> (String,Scenario) -- ^ hypothetical
                           -> ExplainIO EOTaxMethod
chooseEffectiveEOTaxMethod (titleA,scActual) (titleB,scHypo) = do
  explain "effectiveEOTaxMethod" $ "we need to determine if there is aggregation of income. let's see if the different treatments matter. we will compare 34.1 with 2.3, considering the actual (" ++ titleA ++ ") and hypothetical (" ++ titleB ++ ") scenarios"
  treatmentA <- ("section 34.1",) <$> aggregationOfIncome section_34_1 scActual scHypo
  treatmentB <- ("section 2.3",)  <$> aggregationOfIncome section_2_3  scActual scHypo
  explain "effectiveEOTaxMethod" $ "under treatment " <> fst treatmentA <> ", there is" ++ (if snd treatmentA then " " else " not ") ++ "aggregation of income."
  explain "effectiveEOTaxMethod" $ "under treatment " <> fst treatmentB <> ", there is" ++ (if snd treatmentB then " " else " not ") ++ "aggregation of income."
  if snd treatmentA
  then explain "effectiveEOTaxMethod" "there is aggregation of income, so the one-fifths method is indicated." >> return EOFifth
  else explain "effectiveEOTaxMethod" "there is not aggregation of income, so the normal method is indicated." >> return EONormal

aggregationOfIncome :: (Scenario -> ExplainIO Scenario)
                    -> Scenario
                    -> Scenario
                    -> ExplainIO Bool
aggregationOfIncome section fired unfired = do
  actual <- section fired
  hypo   <- section unfired
  let tti_actual = cell actual "total taxable income" "total"
      tti_hypo   = cell hypo   "total taxable income" "total"
      ttp_actual = cell actual "total tax payable"    "total"
      ttp_hypo   = cell hypo   "total tax payable"    "total"
  explain "aggregationOfIncome" $ "in the actual scenario, total taxable income is " ++ show tti_actual ++ " and total tax payable is = " ++ show ttp_actual
  explain "aggregationOfIncome" $ "in the hypo   scenario, total taxable income is " ++ show tti_hypo   ++ " and total tax payable is = " ++ show ttp_hypo
  case tti_actual `compare` tti_hypo of
    GT -> explain "aggregationOfIncome" "actual > hypo, returning true"  >> return True
    EQ -> explain "aggregationOfIncome" "actual = hypo, returning false" >> return False
    LT -> explain "aggregationOfIncome" "actual < hypo, returning false" >> return False

-- | We set up an "explanation monad" which is basically a State+IO
-- monad. The state component is a Data.Tree. We wrap child executions
-- within a mkNod wrapper which behaves mostly as a Writer monad,
-- except we want to give each computation a history of prior
-- executions that just might affect it, so we make it a State. In the
-- future it would be nice to wrap the IO computations so that any
-- @putStrLn@s get automatically wrapped with a @#+begin_example / #+end_example@ block.

type Explanation   = [(String, String)]
type ExplainIO = StateT Explanation IO

explain :: String -> String -> StateT [(String, String)] IO ()
explain foo bar = do
  modify (++ [(foo,bar)])
  -- liftIO (putStrLn $ "- " ++ foo ++ " :: " ++ bar)

runExplainIO :: StateT [([Char], [Char])] IO a -> IO (a, Explanation)
runExplainIO x = runStateT x []
  
printExplanation :: [([Char], [Char])] -> IO ()
printExplanation xs = do
  putStrLn . unlines . fmap (\(a,b) -> "- " ++ a ++ " :: " ++ b) $ xs
  
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
data Replace = Replace { elems_  :: [String] , with_ :: [String] }

-- | run the generic replacement for Scenarios
runReplaceSc :: Scenario -> Replace -> ExplainIO Scenario
runReplaceSc m (Replace ks fs) =
  if all (`Map.member` m) ks
  then do
    nu <- sequence $ metaFsc <$> fs <*> [m]
    return $ Map.unions $ nu ++ [foldl (flip Map.delete) m ks]
  else return m

runReplaceIs :: IncomeStreams -> Replace -> ExplainIO IncomeStreams
runReplaceIs m (Replace ks fs) =
  if all (`Map.member` m) ks
  then Map.unions . (++ [foldl (flip Map.delete) m ks]) <$> sequence ((metaFis <$> fs) <*> [m]) 
  else return m

-- | syntactic sugar for setting up a Replace transformation
(~->) :: [String] -> [String] -> Replace
(~->) k ka = Replace { elems_ = k, with_ = ka }

-- | generic section wrapper
runSection :: String -> Scenario -> [Replace] -> ExplainIO Scenario
runSection name initialScenario transformations = do
  liftIO $ putStrLn $ "** executing " <> name
  steps <- scanlM runReplaceSc initialScenario transformations
  _ <- liftIO $ sequence [ putStrLn ("*** step " <> show n) >> putStrLn (asExample step)
                         | (n, step) <- zip [1::Int ..] steps ]
  return $ last steps

-- | section 2(3) EStG
section_2_3 :: Scenario -> ExplainIO Scenario
section_2_3 sc = do
  runSection "section 2.3" sc
        [ []                                             ~-> []
        , ["ordinary income", "extraordinary income"]    ~-> ["squashIncomes"]
        , ["combined income", "ordinary expenses"
          ,"special expenses"]                           ~-> ["netIncome"]          -- 2_3_2
        , ["net income"]                                 ~-> ["offsetLosses_2_3_3"] -- 2_3_3
        , []                                             ~-> ["squashCats"]
        , []                                             ~-> ["ordinaryPayable"]
      -- marital adjustments
      -- carryover loss, if net negative then leave some negative?
        ]

-- | run through a specific set of transformations defined in section 34_1.
-- 
-- A particular transformation runs only if all its LHS columns are found in the scenario table,
-- in which case those columns are replaced by the output of the transformer.
section_34_1 :: Scenario -> ExplainIO Scenario
section_34_1 sc = do
  runSection "section 34.1" sc
        [ []                                            ~-> []
        , ["ordinary income", "ordinary expenses"
        , "special expenses"]                           ~-> ["preNetIncome"]
        , ["pre-net income"]                            ~-> ["offsetLosses"]
        , []                                            ~-> ["squashCats"]
        , []                                            ~-> ["extraordinary"]
        , []                                            ~-> ["sentence3"]
        , ["1 revised RTI taxation due to sentence 3"]  ~-> ["sentence3_b"]
        , [ "1 RTI taxation"
          , "2 RTI plus one fifth"
          , "3 tax on RTI+.2"
          , "4 difference"
          , "5 extraordinary taxation"
          , "extraordinary income"
          , "remaining taxable income"]                 ~-> ["totalPayable"]
        ]



-- | a meta-function constructor for use by our meta-interpreter.
-- Instead of just a direct function definition @f = whatever@
-- we have (generally) @metaF "f" = whatever@ which gets called later.
-- here, the @whatever@ is a Scenario, so we say `metaFsc`.
metaFsc :: String -> Scenario -> ExplainIO Scenario

metaFsc "preNetIncome" sc = return $
  Map.singleton "pre-net income" $
  mapAp (-) (sc Map.! "ordinary income") $
  mapAp (+) (sc Map.! "special expenses") (sc Map.! "ordinary expenses")

metaFsc "netIncome" sc = return $
  Map.singleton "net income" $
  mapAp (-) (sc Map.! "combined income") $
  mapAp (+) (sc Map.! "special expenses") (sc Map.! "ordinary expenses")

-- | squash extraordinary into pre-net income; only used for section 2
metaFsc title@"squashIncomes" sc = do
  sequence_ [ explain title $ "for " <> k <> ", " <>
              "extraordinary " ++      scell sc "extraordinary income"       k ++
              " + ordinary "   ++      scell sc "ordinary income"            k ++
              " = pre-net "    ++ show (cell sc "extraordinary income"       k +  cell sc "ordinary income" k)
            | k <- Map.keys $ sc Map.! "extraordinary income"
            , sc Map.! "extraordinary income" Map.! k /= 0
            ]
  return $
    Map.singleton "combined income" $
    mapAp (+) (sc Map.! "ordinary income") (sc Map.! "extraordinary income")


-- | loss offsets, based on section 34.
-- losses from one income category can be used to offset earnings in another.
-- the offsetting is done on a per-category basis, against the single "pre-net income" column, pro rata
--
metaFsc "offsetLosses" sc = return $
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
metaFsc title@"offsetLosses_2_3_3" sc = do
  let orig = sc Map.! "net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum $ Map.elems negatives
      totalPos = sum $ Map.elems positives
      maxReduction = if   totalPos > 100000
                     then totalNeg / 2
                     else totalNeg
      reductio = 1 + max (-1) (maxReduction / totalPos)
  when (totalPos > 100000) $ do
    explain title $ "sum of the positive incomes " ++ show totalPos ++ " exceeds 100000"
    explain title $ "so we will limit deductions to half of the sum of the negative incomes " ++ show totalNeg ++ " = " ++ show maxReduction
    explain title $ "and apply them pro rata to the positive incomes"
  when (totalPos <= 100000) $ do
    explain title $ "sum of the positive incomes " ++ show totalPos ++ " is less than 100000"
    explain title $ "so we will not limit deductions to half of the sum of the negative incomes; the deductible amount will be " ++ show maxReduction
    explain title $ "we will apply deductions pro rata to the positive incomes"
  explain title $ "reductio = " <> show reductio
  rti <- sequence [ (cat,) <$> offsetLosses_2_3_4 cat reductio n
                  | (cat,n) <- Map.toList orig ]
  -- viaprorata <- prorateF (* reductio) (>0) 0 (Map.elems orig)
  -- liftIO $ putStrLn $ "via pro rata, we would have " ++ show viaprorata
  return $ Map.singleton "total taxable income" (Map.fromList rti)
    
  where 
    -- | based on section 2(3) para 4
    -- 
    -- 4. The reduction is to be made in proportion to
    --    1. the positive totals of income
    --       1. from different types of income
    --    2. to the total of positive income.
    offsetLosses_2_3_4 :: String -> Float -> Float -> ExplainIO Float
    offsetLosses_2_3_4 categoryName reduction x
      | x < 0     = pure 0 <* explain title (categoryName <> " is negative, resetting to 0")
      | x == 0    = pure 0
      | otherwise = do
          let out = x * reduction
          explain title (categoryName <> " " <> show x <> " is positive, multiplying by " ++ show reduction ++ " = " ++ show out)
          return out

-- | extraordinary income is taxed
metaFsc "extraordinary" sc = return $
  let ordinary     = sc Map.! "remaining taxable income"
      zeroOrdinary = max 0 <$> ordinary
      extraI       = sc Map.! "extraordinary income"
      ordtax       = taxFor zeroOrdinary
      totalI       = mapAp (+) zeroOrdinary extraI
      rti5         = mapAp (+) zeroOrdinary ((/5) <$> extraI)
      rti5tax      = max 0 <$> taxFor rti5
      delta        = abs <$> mapAp (-) ordtax rti5tax
      rti5tax5     = (5*) <$> delta
      
  in Map.fromList [("1 RTI taxation",           ordtax)
                  ,("2 RTI plus one fifth",     rti5)
                  ,("3 tax on RTI+.2",          rti5tax)
                  ,("4 difference",             delta)
                  ,("5 extraordinary taxation", rti5tax5)
                  ,("total taxable income",     totalI)
                  ]
  
-- | sentence 3
metaFsc "sentence3" sc = return $
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

metaFsc "sentence3_b" sc = return $
  Map.fromList [("1 RTI taxation", sc Map.! "1 revised RTI taxation due to sentence 3")]


metaFsc "totalPayable" sc = return $
  let rtiTax   = sc Map.! "1 RTI taxation"
      extraTax = sc Map.! "5 extraordinary taxation"
  in Map.fromList [("total tax payable", mapAp (+) rtiTax extraTax)]

metaFsc "ordinaryPayable" sc = return $
  let ttiTax   = taxFor $ sc Map.! "total taxable income"
  in Map.fromList [("total tax payable", ttiTax)]

-- | squash all non-exempt income categories together
metaFsc "squashCats" sc = sequence $
  (\istream -> runReplaceIs istream (categoryKeys ~-> ["squashCats'"])) <$> sc
  where
    categoryKeys :: [String]
    categoryKeys = nub (concatMap Map.keys (Map.elems sc))

metaFsc fname _ = return $ error $ "metaFsc called for undefined function name " <> fname

metaFis :: Num a => String -> Map.Map String a -> ExplainIO (Map.Map String a)
metaFis "squashCats'" ns = return $
  Map.singleton "total" $ sum $ Map.elems $ Map.filterWithKey (\k _ -> not ("Exempt " `isPrefixOf` k)) ns

metaFis fname _ = return $ error $ "metaFis called for undefined function name " <> fname

-- | what does pro rata mean?
-- it means we map some function across a functor, where each application is scaled to that value's fraction of the whole
--
-- example: @ prorate (* 1000) [5,3,2] == [500.0,300.0,200.0] @ 
prorate :: (Fractional a, Functor f, Foldable f) => (a -> a) -> f a -> ExplainIO (f a)
prorate f xs = return $ f . (/ sum xs) <$> xs

-- | what does filtered pro rata mean?
-- it means we map some function across a functor, where each application is scaled to that value's fraction of some partitioned subset of the functor, which passes a filter
-- elements which fail that filter are reset to mempty
-- example: divide 1000 new shares among all the debtholders who hold a positive balance, while zeroing the allotment for any who hold a negative balance
-- @
--   prorateF (const 1000) (>0) [5,3,2,-1] ==> [500.0,300.0,200.0,0]
-- @
-- 
-- example: halve the value of all elements which are positive, while zeroing anything negative
-- @
--   prorateF (/ 2) (>0) [5,3,2,-1] ==> [500.0,300.0,200.0,0]
-- @
-- 
prorateF :: (Fractional a, Functor f, Foldable f) => (a -> a) -> (a -> Bool) -> a -> f a -> ExplainIO (f a)
prorateF f filt zeroval xs = do
  let passes = sum $ (\x -> if filt x then x else zeroval) <$> xs
  return $ (\x -> if filt x
                  then f x
                  else zeroval) <$> xs

-- map application
mapAp :: Ord k => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a
mapAp = Map.unionWith

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

-- | we'll use the 2023 table when computing taxes.
taxFor :: Map.Map IncomeCategory Float -> Map.Map IncomeCategory Float
taxFor       = mapOnly (>0) (progDirect 2023)

-- | Direct computation of progressive tax, without recursing to lower tiers of the stack.
-- this is for countries which just give a direct formula that already takes into account
-- the lower tiers of the stack. the @go@ function here just looks up the correct formula
-- based on the taxable income tier.
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
      | income' `compare` n == o = r income' / 100 * (income' - n) + progRate rts n
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

-- | monadic scanl, but not very performant:
-- 
-- > 00:12 < c_wraith> But it matters a lot less with than with scanl because this will probably fall apart on large
-- >                   lists anyway
-- > 00:12 < c_wraith> it's already not capable of streaming results like scanl in general
-- > 00:14 < c_wraith> this one is tough to do a more detailed strictness analysis on, because it depends on (>>=)
-- > 00:15 < c_wraith> I think there is *a* case where forcing t before the recursive call can matter.
-- > 00:16 < c_wraith> yeah, if the monad's (>>=) is sufficiently lazy, like Identity, it can matter if you're
-- >                   skipping elements of the result list.
-- > 00:16 < [exa]> c_wraith: e.g. for Identity this becomes a normal scanl, but I'd say weird stuff may start
-- >                happening with say State.Lazy
-- > 00:16 < c_wraith> [exa]: that stuff is the responsibility of (>>=) to handle.
-- > 00:16 < freeside> i am very much hoping to engage in weird activity in the near future, but not today. I mean
-- >                   LogicT.
-- > 00:17 < [exa]> LogicT <3
-- > 00:17 < [exa]> anyway yeah a bit of `seq` never hurt nobody, right?
-- > 00:18 < c_wraith> definitely false.  I've had people think they were optimizing libraries I was using and break
-- >                   them.
-- > 00:18 < c_wraith> like...  bytestring
-- > 00:18  * [exa] was joking, yeah
-- > 00:19 < c_wraith> still, given the recursive structure, I think scanlM' (forcing t before the recursive call), is
-- >                   almost always more appropriate.
-- > 
scanlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM _ _ []     = return []
scanlM f q (x:xs) = do
  t  <- f q x
  ts <- scanlM f t xs
  return $ t `seq` (t : ts)

scell :: (Ord k, Ord j, Show a) => Map.Map k (Map.Map j a) -> k -> j -> String
scell k j a = show (cell k j a)

cell :: (Ord k, Ord j) => Map.Map k (Map.Map j a) -> k -> j -> a
cell k j a = k Map.! j Map.! a

