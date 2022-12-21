module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State ( get, gets, State, StateT, evalState, evalStateT, runState, runStateT )
import Control.Monad.State (liftIO)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char ( string, char, numberChar, alphaNumChar, hspace, space, newline )
import Text.Parsec.Combinator hiding (choice, optional)
import Data.List
import Data.Maybe (fromMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

tryChoice = choice . (fmap try)

toEng :: MathLang a -> String
toEng x = "[TODO]"

int :: Parser Int
int = read <$> some numberChar


runTests :: IO ()
runTests = do
  let symtab = Map.fromList [("foo", (VarMath (MathVal (5 :: Float))))]
  let test1 = evalState (evalMath (MathITE
                                   (PredGt (MathVal 1) (MathVal 2))
                                   (MathVar "foo")
                                   (MathVal 6))) symtab :: Float
  print test1

  runStateT section_34_1 (defaultScenario { grossIncome = Map.fromList [(CatRents, ByCategory 0 72150)] })
  return ()
  
--  let parsed = runParser pMathLang "" "( 5 + 3 * 2 )"
--  print parsed

type IncomeStreams = Map.Map IncomeCategory ByCategory

data IncomeCategory
  = CatAgriculture
  | CatTrade
  | CatIndependent
  | CatEmployment
  | CatCapital
  | CatRents
  | CatOther
  deriving (Eq, Show, Ord)

data ByCategory = ByCategory
  { extraOrdinary :: Int
  , ordinary :: Int
  }
  deriving (Eq, Show)

type NetIncome = Int
type TaxableIncome = Int

data Scenario = Scenario
  { grossIncome       :: IncomeStreams
  , expenses          :: IncomeStreams
  , lumpSumDeductions :: IncomeStreams
  , specialExpenses   :: IncomeStreams
  }
  deriving (Eq, Show)

section_34_1 :: StateT Scenario IO Scenario
section_34_1 = do
  scenario <- get
  liftIO $ putStrLn "* initial scenario"
  liftIO $ putStrLn "** initial income streams"
  liftIO $ print $ grossIncome scenario
  liftIO $ putStrLn "** initial expenses"
  liftIO $ print $ expenses scenario

  let income1 :: IncomeStreams
      income1 = lessExpenses scenario
        
  let netIncome :: IncomeStreams
      netIncome = offsetLosses income1

  liftIO $ putStrLn "** net income after offsetting losses"
  liftIO $ print $ netIncome
  
  let taxableIncome :: IncomeStreams
      taxableIncome = furtherReduce netIncome
  liftIO $ putStrLn "** taxable income after further reductions"
  liftIO $ print $ taxableIncome
  return $ defaultScenario { grossIncome = taxableIncome }

defaultScenario :: Scenario
defaultScenario =
  Scenario { grossIncome = Map.empty
           , expenses = Map.empty
           , lumpSumDeductions = Map.empty
           , specialExpenses = Map.empty
           }

  

offsetLosses :: IncomeStreams -> IncomeStreams
offsetLosses orig =
  let (negatives, positives) = partition (\(c,i) -> ordinary i < 0) (Map.toList orig)
      totalNeg = sum (ordinary . snd <$> negatives)
      totalPos = sum (ordinary . snd <$> positives)
  in
    -- [TODO] reduce each of the positives by a pro-rated amount
    orig

-- [TODO] merge the extraordinary and ordinary streams of income
furtherReduce :: IncomeStreams -> IncomeStreams
furtherReduce orig = orig

lessExpenses :: Scenario -> IncomeStreams
lessExpenses sc =
  grossIncome sc `minus` expenses sc
  where
    minus :: IncomeStreams -> IncomeStreams -> IncomeStreams
    minus is1 is2 =
      Map.fromList [ (ic, bc)
                   | (ic, bc1) <- Map.toList is1
                   , let bc = ByCategory 0 $ ordinary bc1 - ordinary (fromMaybe (ByCategory 0 0) (Map.lookup ic is2))
                   ]

