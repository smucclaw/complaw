{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State ( get, gets, State, StateT, evalState, evalStateT, runState, runStateT )
import Control.Monad.State (liftIO)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char ( string, char, numberChar, alphaNumChar, hspace, space, newline )
import Text.Parsec.Combinator hiding (choice, optional)
import Data.List
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Boxes hiding ((<>))
import qualified Text.PrettyPrint.Boxes as BX
import Data.Ord

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
pMathLang :: (Fractional a, Ord a) => Parser (MathLang a)
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


runTests :: IO ()
runTests = do
  let symtab = Map.fromList [("foo", VarMath (MathVal (5 :: Float)))]
  let test1 = evalState (evalMath (MathITE
                                   (PredGt (MathVal 1) (MathVal 2))
                                   (MathVar "foo")
                                   (MathVal 6))) symtab :: Float
  print test1
  let startScenario =
        Map.update (pure
                     . Map.update (const $ pure   72150) "Rents"
                   )
        "ordinary income" $
        Map.update (pure
                     . Map.update (const $ pure    2150) "Rents"
                     . Map.update (const $ pure   10000) "Other"
                   )
        "ordinary expenses" $
        defaultScenario

  _ <- runStateT section_34_1 startScenario
  return ()
  
--  let parsed = runParser pMathLang "" "( 5 + 3 * 2 )"
--  print parsed

type Scenario      = Map.Map String         IncomeStreams
type IncomeStreams = Map.Map IncomeCategory Float

type IncomeCategory = String

incomeCategories :: [IncomeCategory]
incomeCategories =
  [ "Agriculture"
  , "Trade"
  , "Independent"
  , "Employment"
  , "Capital"
  , "Rents"
  , "Other"
  ]

type NetIncome     = Int
type TaxableIncome = Int

-- | render a Scenario as a table -- something like this:
-- @
--                extraordinary income  lump sum deductions  ordinary expenses  ordinary income  special expenses
--   Agriculture  0                     0                    0                  0                0
--   Capital      0                     0                    0                  0                0
--   Employment   0                     0                    0                  0                0
--   Independent  0                     0                    0                  0                0
--   Other        0                     0                    0                  0                0
--   Rents        0                     0                    0                  72150            0
--   Trade        0                     0                    0                  0                0
-- @

asTable :: Scenario -> Box
asTable sc =
  hsep 2 BX.left (
  -- row headers at left
  vcat BX.left (emptyBox 1 1 : (BX.text <$> sort incomeCategories))
    : [ vcat BX.left (
          -- column headers at top
          BX.text streamName
            : [ BX.text (show (round numval :: Int))
              | numval <- Map.elems streamVal -- should be sorted by incomeCategory i think
              ] )
      | (streamName, streamVal) <- Map.toAscList sc
      ]
  )

asColumn str istream = asTable (Map.fromList [(str, istream)])

_with, _in :: ()
_with = ()
_in = ()
_given_by = ()

data Replacement k a = Replace
  { columns_  :: [k]
  , with_     :: k
  , given_by_ :: Map.Map k a -> a
  }

runReplace :: Ord k => Replacement k a -> Map.Map k a -> Map.Map k a
runReplace (Replace ks newk f) m = Map.insert newk (f m) $ foldl (flip Map.delete) m ks

section_34_1 :: StateT Scenario IO Scenario
section_34_1 = do
  scenario <- get
  liftIO $ putStrLn "* initial scenario"
  liftIO $ putStrLn $ render ( asTable scenario )

  let income1 :: Replacement String IncomeStreams
      income1 = Replace { columns_  = ["ordinary income", "ordinary expenses"]
                        , with_     = "pre-net income"
                        , given_by_ = lessExpenses }
  liftIO $ putStrLn $ render $ asTable $ runReplace income1 scenario

  let income2 :: Replacement String IncomeStreams
      income2 = Replace { columns_  = ["pre-net income"]
                        , with_     = "post-offset income"
                        , given_by_ = offsetLosses }
  liftIO $ putStrLn $ render $ asTable $ runReplace income2 $ runReplace income1 scenario

  return $ runReplace income2 $ runReplace income1 $ scenario

--  let netIncome :: Scenario
--      netIncome = replace offsetLosses income1
--
--  liftIO $ putStrLn "** net income after offsetting losses"
--  liftIO $ print $ netIncome
--  
--  let taxableIncome :: IncomeStreams
--      taxableIncome = furtherReduce netIncome
--  liftIO $ putStrLn "** taxable income after further reductions"
--  liftIO $ print $ taxableIncome
--  return $ Map.update (const $ pure taxableIncome) "ordinary Income" defaultScenario



lessExpenses :: Scenario -> IncomeStreams
lessExpenses sc =
  Map.unionWith (-) (sc Map.! "ordinary income") (sc Map.! "ordinary expenses")

-- | losses from one income category can be used to offset earnings in another.
-- the offsetting is done on a per-category basis, against the single "pre-net income" column
offsetLosses :: Scenario -> IncomeStreams
offsetLosses sc =
  let orig = sc Map.! "pre-net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum (Map.elems negatives)
      totalPos = sum (Map.elems positives)
  in
    (\x -> if x < 0
           then 0
           else x + totalNeg * (x / totalPos))
    <$> orig 

-- [TODO] merge the extraordinary and ordinary streams of income
furtherReduce :: IncomeStreams -> IncomeStreams
furtherReduce orig = orig

defaultScenario :: Scenario
defaultScenario =
  Map.fromList [ (i, defaultStream) | i <- ["ordinary income"
                                           ,"extraordinary income"
                                           ,"ordinary expenses"
                                           ,"special expenses"
                                           ,"lump sum deductions"
                                           ] ]

defaultStream :: IncomeStreams
defaultStream = Map.fromList [(ic,0) | ic <- incomeCategories]

-- | fmap only those elements that qualify, leaving the others alone
mapOnly :: Functor t => (a -> Bool) -> (a -> a) -> t a -> t a
mapOnly test transform items = (\i -> if test i then transform i else id i) <$> items

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

rateTable :: (Fractional a) => Int -> [(Ordering, a, a)]
rateTable 2022 = [(LT,   9409,  0)
                 ,(GT,   9408, 14)
                 ,(GT,  57051, 42)
                 ,(GT, 270500, 45)
                 ]
rateTable _    = rateTable 2022

getRate :: (Ord a, Fractional a) => Int -> a -> a
getRate year income =
  let rt = reverse $ rateTable year
  in progRate rt income
  where
    progRate ((o,n,r):rts) income
      | income `compare` n == o = r/100 * (income - r) + progRate rts n
      | otherwise               = progRate rts income
    progRate [] _ = 0

