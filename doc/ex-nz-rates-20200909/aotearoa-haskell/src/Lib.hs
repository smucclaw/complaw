{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( evalMath
    , runScenario
    , goals
    , nlgstyles
    , loadTestFile
    , Verbosity(VNeutral,VConcrete,VAbstract,VObfuscated,VAlgebra,VYAML,VBool,VXAlgo,VCicero)
    , SomeFact(FS,FD)
    , Environment(Env)
    , variables_dict
    , RatesRebateWorld
    , deobfuscate, obfuscate
    , MathExpr(Lit)
    ) where

import BoolExpr
import qualified Ling
-- import qualified Data.Yaml as Y
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import Data.Maybe
import Text.Printf
import Text.Regex
import Control.Monad.Reader
import Debug.Trace

variables_dict :: NLdict
variables_dict = Map.fromList [("en",
                                Map.fromList [
    ("rates_total",              NLAttr UserVar "the rates payable for that rating year in respect of the property")
  , ("combined_income",          NLAttr UserVar "the ratepayer's income for the preceding tax year")
--, ("dependants",               NLAttr UserVar "the number of persons who were a dependant of the ratepayer at the commencement of the rating year in respect of which the application is made")
  , ("dependants",               NLAttr UserVar "person who was a dependant of the ratepayer")
  , ("initial_contribution",     NLAttr LegislativeParam "the initial contribution by ratepayer")
  , ("additional_per_dependant", NLAttr LegislativeParam "the additional allowable income per dependant")
  , ("income_threshold",         NLAttr LegislativeParam "the income threshold")
  , ("maximum_allowable",        NLAttr LegislativeParam "the maximum rebate allowed")
  , ("excess_rates_amount",      NLAttr BitOfBoth "the excess rates amount")
  , ("income_taper_amount",      NLAttr BitOfBoth "the income taper amount")
  , ("income_taper_trigger",     NLAttr BitOfBoth "the income taper trigger")
  ])]

type BoolGoal = Map.Map String (MathExpr BoolExpr)
boolgoals :: BoolGoal
boolgoals = Map.fromList [("varun2", (Mul (Descr "PRINCIPAL_CAREGIVER") (Sum (Sum (Descr "is_the_biological_mother") (Mul (Descr "is_spouse_or_partner_of_the_biological_mother") (Descr "has_spouse_who_transferred_her_entitlement"))) (Mul (Descr "a_person_other_than_biological_mother_or_her_spouse") (Descr "taking_permanent_primary_responsibility_for_child")))))
            ]


type Goals = Map.Map String (MathExpr Double)
goals :: Goals
goals = Map.fromList [("math1", (Mul (Div (Lit 1.0) (Lit 2.0)) (Lit 8.0)))
                     ,("math2", (Mul (Div (Lit 1.0) (Lit 2.0)) (Descr "week_numDays")))
                     -- http://www.legislation.govt.nz/act/public/1973/0005/latest/whole.html#DLM409673
                     ,("varun1", (Max (Lit 0) (Min (Sub (Sub (Descr "rates_total") (Descr "initial_contribution")) (Sum (Div (Sub (Descr "rates_total") (Descr "initial_contribution")) (Lit 3)) (IntDiv (Sub (Descr "combined_income") (Sum (Mul (Descr "dependants") (Descr "additional_per_dependant")) (Descr "income_threshold"))) (Lit 8)))) (Descr "maximum_allowable"))))

-- A ratepayer who, at the commencement of a rating year, was the ratepayer of a residential property is entitled, on application in that year, to a rebate of—
    -- (a) so much of the rates payable for that rating year in respect of the property as represents—
    --     (i) two-thirds of the amount by which those rates exceed $160, reduced by—
    --     (ii) $1 for each $8 by which the ratepayer’s income for the preceding tax year exceeded $25,180, that last-mentioned amount being increased by $500 in respect of each person who was a dependant of the ratepayer at the commencement of the rating year in respect of which the application is made; or
    -- (b) $630,—
    -- whichever amount is smaller.
                       
                     ]

type NatLang = String -- locale string? "en-us"

-- let's try to model the rates rebate act 1973!
data SomeFact = FS Text.Text
              | FD Double
              | FB Bool
              | FM (MathExpr Double) -- should we allow FM or should we keep MathExprs separate from Facts?
              deriving (Ord, Eq, Show)
type RatesRebateWorld = Map.Map SomeFact SomeFact

data Environment = Env { verbosity :: Maybe Verbosity
                       , facts :: RatesRebateWorld
                       , nldict :: NLdict
                       }
                   deriving Show

data NLExpr = FLS NatLang String -- later we can make this more complicated with gender and other cases

type NLDict = Map.Map SomeFact NLExpr

-- TODO next: pass a dictionary of english expressions alongside, upgrade everythin to a monad reader, jam both the fact world and the dictionary together in the same environment

-- we can explain the rule in the abstract (intensionally, by giving constitutive definitions)
-- we can explain the rule concretely (extensionally, filling in known values wherever possible)
data Verbosity = VObfuscated | VAbstract | VNeutral | VConcrete | VAlgebra Int | VYAML | VBool | VXAlgo | VCicero
  deriving (Ord, Eq, Show)

rates_rebate__1 :: RatesRebateWorld -> Double
rates_rebate__1 world = 0.0


type MathVarName = Text.Text

data MathExpr a = Lit                        a               -- literal double
                | Descr             MathVarName              -- to be looked up in the variable symbol tables
                | ObfuscatedSum    (MathExpr a) (MathExpr a) -- x + y
                |           Sum    (MathExpr a) (MathExpr a) -- x + y
                | ObfuscatedSub    (MathExpr a) (MathExpr a) -- x - y
                |           Sub    (MathExpr a) (MathExpr a) -- x - y
                | Inxs             (MathExpr a) (MathExpr a) -- clip x - y to 0; max(0, x-y); x-y may not be negative
                | Inxs_            (MathExpr a) (MathExpr a) -- clip x - y to 0; max(0, x-y); x-y may not be negative
                |   ReducedBy      (MathExpr a) (MathExpr a) -- similar to Sub x y but with a few semantics around inequality and negativity
                | ObfuscatedMul    (MathExpr a) (MathExpr a) -- x * y
                |           Mul    (MathExpr a) (MathExpr a) -- x * y
                | IntDiv           (MathExpr a) (MathExpr a) -- x div y, integer division
                |    Div           (MathExpr a) (MathExpr a) -- x / y
                | ObfuscatedMin    (MathExpr a) (MathExpr a) -- min x y
                |           Min    (MathExpr a) (MathExpr a) -- min x y
                |   SoMuchOf       (MathExpr a) (MathExpr a) -- alias to min x y
                | ObfuscatedMax    (MathExpr a) (MathExpr a) -- min x y
                |           Max    (MathExpr a) (MathExpr a) -- max x y
                | Labeled MathVarName (MathExpr a) -- let varFive = (2+3)  or perhaps   varFive@(2+3)
                deriving (Ord, Eq, Show, Read)

-- maybe let's make MathExpr a Traversable or Functor or something so we can just map things like obfuscate and deobfuscate on it
deobfuscate :: MathExpr Double -> MathExpr Double
deobfuscate (Lit           x)   = (Lit x)
deobfuscate (Descr         x)   = (Descr x)
deobfuscate (ObfuscatedSum x y) = (Sum                (deobfuscate x) (deobfuscate y))
deobfuscate (Sum           x y) = (Sum                (deobfuscate x) (deobfuscate y))
deobfuscate (ObfuscatedSub x y) = (Sub                (deobfuscate x) (deobfuscate y))
deobfuscate (Sub           x y) = (Sub                (deobfuscate x) (deobfuscate y))
deobfuscate (Inxs          x y) = (Inxs               (deobfuscate x) (deobfuscate y))
deobfuscate (Inxs_         x y) = (Inxs_              (deobfuscate x) (deobfuscate y))
deobfuscate (ReducedBy     x y) = (Max (Lit 0.0) (Sub (deobfuscate x) (deobfuscate y)))
deobfuscate (ObfuscatedMul x y) = (Mul                (deobfuscate x) (deobfuscate y))
deobfuscate (Mul           x y) = (Mul                (deobfuscate x) (deobfuscate y))
deobfuscate (IntDiv        x y) = (IntDiv             (deobfuscate x) (deobfuscate y))
deobfuscate (Div           x y) = (Div                (deobfuscate x) (deobfuscate y))
deobfuscate (ObfuscatedMin x y) = (Min                (deobfuscate x) (deobfuscate y))
deobfuscate (Min           x y) = (Min                (deobfuscate x) (deobfuscate y))
deobfuscate (SoMuchOf      x y) = (Min                (deobfuscate x) (deobfuscate y))
deobfuscate (ObfuscatedMax x y) = (Max                (deobfuscate x) (deobfuscate y))
deobfuscate (Max           x y) = (Max                (deobfuscate x) (deobfuscate y))
deobfuscate (Labeled     s x  ) =                      deobfuscate x

-- for the lulz
obfuscate :: MathExpr Double -> MathExpr Double
obfuscate (Lit               x)           = (Lit x)
obfuscate (Descr             x)           = (Descr x)
obfuscate (ObfuscatedSum     x y)         = (ObfuscatedSum  (obfuscate x) (obfuscate y))
obfuscate (Sum               x y)         = (          Sum  (obfuscate x) (obfuscate y))
obfuscate (ObfuscatedSub     x y)         = (ObfuscatedSub  (obfuscate x) (obfuscate y))
obfuscate (Sub               x y)         = (          Sub  (obfuscate x) (obfuscate y))
obfuscate (Inxs              x y)         = (Inxs           (obfuscate x) (obfuscate y))
obfuscate (Inxs_             x y)         = (Inxs_          (obfuscate x) (obfuscate y))
obfuscate (ReducedBy         x y)         = (ReducedBy      (obfuscate x) (obfuscate y))
obfuscate (ObfuscatedMul     x y)         = (ObfuscatedMul  (obfuscate x) (obfuscate y))
obfuscate (Mul               x y)         = (Mul            (obfuscate x) (obfuscate y))
obfuscate (IntDiv            x y)         = (IntDiv         (obfuscate x) (obfuscate y))
obfuscate (Div               x y)         = (Div            (obfuscate x) (obfuscate y))
obfuscate (ObfuscatedMin     x y)         = (ObfuscatedMin  (obfuscate x) (obfuscate y))
obfuscate (Min       x@(Lit _) y)         = (SoMuchOf       (obfuscate x) (obfuscate y))
obfuscate (Min               x y@(Lit _)) = (ObfuscatedMin  (obfuscate x) (obfuscate y))
obfuscate (Min               x y)         = (ObfuscatedMin  (obfuscate x) (obfuscate y))
obfuscate (SoMuchOf          x y)         = (SoMuchOf       (obfuscate x) (obfuscate y))
obfuscate (ObfuscatedMax     x y)         = (ObfuscatedMax  (obfuscate x) (obfuscate y))
obfuscate (Max x y)                       = (ObfuscatedMax  (obfuscate x) (obfuscate y))
obfuscate (Labeled         s x  )          = obfuscate x

toAlgebra :: Environment -> MathExpr Double -> Text.Text
toAlgebra env m = let v = verbosity env
                      sho = toAlgebra env
                  in case m of
  (Lit x)             -> showdouble x
  (Descr x)           -> case v of Just (VAlgebra 2) -> equalsFact x (facts env)
                                   Just (VAlgebra 1) -> justNumber x (facts env)
                                   _                 -> x
  (ObfuscatedSum x y) -> sho (Sum x y)
  (ObfuscatedSub x y) -> sho (Sub x y)
  (ObfuscatedMul x y) -> sho (Mul x y)
  (ObfuscatedMin x y) -> sho (Min x y)
  (SoMuchOf x y)      -> sho (Min x y)
  (ObfuscatedMax x y) -> sho (Max x y)
  (Sum x y)           -> "(" <> sho x <> " + "  <> sho y <> ")"
  (Sub x y)           -> "(" <> sho x <> " - "  <> sho y <> ")"
  (Inxs x y)          -> case v of Just (VAlgebra 2) -> "(" <> sho x <> " >- " <> sho y <> ")"
                                   _                 -> "max(0, " <> (sho (Sub x y)) <> " )"
  (Inxs_ x y)         -> sho (Inxs x y)
  (ReducedBy x y)     -> sho (Inxs x y)
  (Mul x y)           -> "(" <> sho x <> " * "  <> sho y <> ")"
  (IntDiv x y)        -> "(" <> sho x <> " // " <> sho y <> ")"  -- or however you represent integer division
  (Div x y)           -> "(" <> sho x <> " / "   <> sho y <> ")"
  (Min x y)           -> "min(" <> sho x <> ", " <> sho y <> ")"
  (Max x y)           -> "max(" <> sho x <> ", " <> sho y <> ")"
  (Labeled s x)       -> sho x

notGiven x w = case Map.lookup (FS x) w of
                 Just (FD y) -> showdouble y
                 _           -> "not given -- you'll be wanting to define " <> x <> ", please."

equalsFact x w = case Map.lookup (FS x) w of
                   Just (FD y) -> "(" <> x <> "=" <> showdouble y <> ")"
                   _           -> x

justNumber x w = case Map.lookup (FS x) w of
                   Just (FD y) -> showdouble y
                   _           -> x

showdouble = Text.pack . printf "%.2f"

-- extract variable definitions into RatesRebateWorld

evalMath :: RatesRebateWorld -> MathExpr Double -> Double
evalMath w (Lit x)   = x
evalMath w (Descr x) = case (Map.lookup (FS x) w) of
                         Just (FD y) -> y
                         Nothing     -> error $ Text.unpack ("in math expression, description " <> x <> " was not found in the RatesRebateWorld")
                         Just _      -> error $ Text.unpack ("in math expression, description " <> x <> " needs to resolve to a Double, but is some other type. :-(")
evalMath w (ObfuscatedSum x y) =         evalMath w (Sum x y)
evalMath w (          Sum x y) =         evalMath w x + evalMath w y
evalMath w (ObfuscatedSub x y) =         evalMath w (Sub x y)
evalMath w (          Sub x y) =         evalMath w x - evalMath w y
evalMath w (Inxs          x y) = max 0 $ evalMath w x - evalMath w y
evalMath w (Inxs_         x y) =         evalMath w (Inxs x y) -- helper function form
evalMath w (ReducedBy     x y) =         evalMath w (Inxs x y)
evalMath w (ObfuscatedMul x y) =         evalMath w (Mul x y)
evalMath w (          Mul x y) =         evalMath w x * evalMath w y
evalMath w (IntDiv        x y) = (fromIntegral ((floor (evalMath w x)) `div` (floor (evalMath w y)))) :: Double
evalMath w (   Div        x y) =         evalMath w x / evalMath w y
evalMath w (ObfuscatedMin x y) =         evalMath w (Min x y)
evalMath w (Min           x y) =    min (evalMath w x) (evalMath w y)
evalMath w (SoMuchOf      x y) =         evalMath w (Min x y)
evalMath w (ObfuscatedMax x y) =         evalMath w (Max x y)
evalMath w (Max           x y) =    max (evalMath w x) (evalMath w y)
evalMath w (Labeled       s x) =         evalMath w x

doublespace = "  "
space x = Text.replicate x doublespace

-- todo: express IdiomaticNum / Labeled in terms of each other, using a transfer function

is_idiomatic x = isJust (transfer_idiomatic x Nothing)

transfer_idiomatic :: MathExpr Double -> Maybe Text.Text -> Maybe Text.Text
transfer_idiomatic indiv@(IntDiv x (Lit y)) ms = Just (Text.unwords $ catMaybes [ dollarprefix (Just 1.00) ms
                                                                                , Just "for each"
                                                                                , dollarprefix (Just y) ms
                                                                                ])
transfer_idiomatic indiv@(Div (Lit x) (Lit y)) ms = case (x,y) of
                                                   (1.0, 2.0) -> Just "one-half"
                                                   (1.0, 3.0) -> Just "one-third"
                                                   (2.0, 3.0) -> Just "two-thirds"
                                                   (3.0, 4.0) -> Just "three-quarters"
                                                   (1.0, 4.0) -> Just "one-quarter"
                                                   _ -> Nothing
transfer_idiomatic x ms = Nothing

-- handle anaphora
--      the goal, believe it or not, is
--            so much of
--            the rates payable for that rating year in respect of the property
--          as represents
--              two-thirds of 
--                the amount by which
--  <<<             the rates payable for that rating year in respect of the property
--  >>>             those rates
--                exceeds
--                  160.00

paragraphTransfer :: Text.Text -> Text.Text
paragraphTransfer inlines = Text.unlines $ Ling.anaphora $ Text.lines $ inlines

dollarprefix :: Maybe Double -> Maybe Text.Text -> Maybe Text.Text
dollarprefix (Just s) (Just "dollar") = Just ("$" <> (chompdecimals $ Text.pack $ show s))
dollarprefix (Just s) (Just y)        = Just (num2en s <> y)
dollarprefix Nothing  (Just y)        = Just         y
dollarprefix (Just s) Nothing         = Just (num2en s)

chompdecimals s = fromMaybe s $ Text.stripSuffix ".0" s

num2en n = words "zero one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty" `mybangbang` (floor n)
  where mybangbang xs k = case drop k xs of
          x:_ -> Text.pack x
          [] ->  Text.pack $ show k

-- todo: upgrade this to a Reader monad, and stuff (v,w) inside it
-- for extra credit, stuff this inside a RWST monad transformer to increment depth
mathToBool :: MathExpr Double -> BoolExpr
mathToBool (Sum x y) = (Or (mathToBool x) (mathToBool y))
mathToBool (Mul x y) = (And (mathToBool x) (mathToBool y))
mathToBool a = error $ "unable to mathToBool" ++ (show a)

boolToAlgebra :: Int -> Environment -> MathExpr BoolExpr -> Text.Text
boolToAlgebra depth env m = let sho = boolToAlgebra (depth+1) env
                      in case m of
  (Sum x y) -> "(" <> sho x <> " || " <> sho y <> ")"
  (Mul x y) -> "(" <> sho x <> " && " <> sho y <> ")"
  (Descr x) -> x
  _         -> "SORRY CAN'T BOOL THIS"

sayMath :: Int -> Environment -> MathExpr Double -> Text.Text
sayMath depth env m = let space0 = space depth
                          space1 = space $ depth + 1
                          space_1 = space $ depth - 1
                          v = verbosity env
                          appropriate_space = if v==Just VConcrete then space0 else space0
                          indent1 (line1,part2) = (space0 <> line1 <>                      part2)
                          indent2 (line1,part2) = (space0 <> line1 <> appropriate_space <> part2)
                          indent_2 (line1,part2) = (         line1 <> appropriate_space <> part2)
                          indent0 = (<>) space0
                          sho = sayWithAnswer (depth+1) env
                       in case m of
  (Sum x y) -> indent2 ("the sum of\n" <> sho x <> "\n",          "with\n" <> sho y)
  (ObfuscatedSum x y) -> indent_2 (sho x <> "\n", ", that last-mentioned amount being increased by\n" <> sho y)
  (ObfuscatedSub x y) -> indent_2 (sho x <> "\n", ", that last-mentioned amount being reduced by\n" <> sho y)
  (Sub x y) -> indent2 ("the difference between\n" <> sho x <> "\n", "and\n" <> sho y)
  (Inxs_ x y) -> indent2 (                    "by which\n" <> sho x <> "\n", "exceeded\n" <> sho y)
  (Inxs  x y) -> indent2 ("the amount by which\n" <> sho x <> "\n", "exceed\n" <> sho y)
  (ObfuscatedMul x y) -> indent_2 (sho x <> "\n", "in respect of each\n" <> sho y)
  (Mul x y) -> if is_idiomatic x
    then let idiomatic_string = fromJust $ transfer_idiomatic x Nothing
         in indent1 (idiomatic_string <> " of \n", sho y)
    else indent2 ("the product of\n" <> sho x <> "\n",           "multiplied by\n" <> sho y)
  (IntDiv x y) -> if is_idiomatic m
    then let idiomatic_string = fromJust $ transfer_idiomatic m $ Just "dollar"
             inxs (Inxs xx yy) = if v /= Just VObfuscated then sho (Inxs xx yy) else sho (Inxs_ xx yy)
             rhs = case x of
               (Inxs xx yy) -> inxs x
               (Labeled ss (Inxs xx yy)) -> inxs (Inxs xx yy)
               _            -> sho x
         in indent1 (idiomatic_string <> (if v /= Just VObfuscated then " in" else "") <> "\n", rhs)
    else indent2 ("the quotient given by integer division of\n" <> sho x <> "\n", "integer divided by\n" <> sho y)

-- TODO: horizontal transfer the (intdiv (inxs)) -> (intdiv (inxs_)) differently

  (Div x y) -> indent2("the quotient given by\n" <> sho x <> "\n", "divided by\n" <> sho y)
  (ObfuscatedMin x y) -> indent2(sho x <> "\n", "; or\n" <> sho y <> "\n" <> space0 <> ", whichever amount is smaller\n")
  (Min x y) -> indent2("the lesser of\n" <> sho x <> "\n", "and\n" <> sho y)
  (SoMuchOf x y) -> indent2("so much of\n" <> sho x <> "\n", "as represents" <> "\n" <> sho y)
  (ReducedBy x y) -> indent_2(sho x <> "\n", "reduced by\n" <> sho y)
  (ObfuscatedMax x y) -> indent2(sho x <> "\n", "; or\n" <> sho y <> ", whichever amount is greater\n")
  (Max x y) -> indent2("the greater of\n" <> sho x <> "\n", "and\n" <> sho y)
  (Lit x)       -> space0 <> showdouble x
  (Labeled s x) -> case verbosity env of
    Just VAbstract -> sayMath depth env x
    Just VObfuscated -> sayMath depth env x
    _              -> let nlsyntax__ = do
                            nlattr <- enlookup s $ nldict env
                            return $ nlsyntax nlattr
                          nlsyntax_ = Data.Maybe.fromMaybe s nlsyntax__
                      in indent1(nlsyntax_ <>", which is" <> "\n", sayMath depth env x)
  (Descr x) -> indent0 $ case verbosity env of
    Nothing          -> ""
    Just VConcrete   -> (Data.Maybe.fromMaybe x (liftM ((x <> ", ") <>) (nlsyntax <$> (enlookup x $ nldict env))))
    Just VNeutral    -> Data.Maybe.fromMaybe x $ do
      nlattr <- (enlookup x $ nldict env)
      let nlsyntax_ = nlsyntax nlattr
          nlporv_   = nlporv   nlattr
      return $ case nlporv_ of
                 LegislativeParam -> nlsyntax_ <> ", which is " <> notGiven x (facts env)
                 UserVar          -> nlsyntax_
                 BitOfBoth        -> x <> " (" <> nlsyntax_ <> " is " <> notGiven x (facts env) <> ")"
    _               -> case nlporv <$> (enlookup x $ nldict env) of
      Just LegislativeParam -> if isJust (Map.lookup (FS x) (facts env)) then chompdecimals $ notGiven x (facts env) else x
      y -> fromMaybe x (nlsyntax <$> (enlookup x $ nldict env))
      

sayWithAnswer :: Int -> Environment -> MathExpr Double -> Text.Text
sayWithAnswer depth env m = let saidMath = sayMath depth env m in
  case m of
    (Lit x)   -> space (depth-1) <> "simply " <> showdouble x
    _         -> answer <> saidMath
  where answer = (if (verbosity env) == Just VConcrete
                  then (space $ depth-1) <> (showdouble $ evalMath (facts env) m) <> " -- which is\n"
                  else "")

-- some examples of things we can say

nlgstyles :: Map.Map Text.Text (Verbosity)
nlgstyles = Map.fromList [("obfuscated", VObfuscated)
                         ,("abstract", VAbstract)
                         ,("neutral",  VNeutral)
                         ,("concrete", VConcrete)
                         ,("algebra",  VAlgebra 0)
                         ,("algebranums",  VAlgebra 1)
                         ,("algebravars",  VAlgebra 2)
                         ,("yaml",     VYAML)
                         ,("bool",     VBool)
                         ,("cicero",   VCicero)
                         ,("xalgo",    VXAlgo)
                         ]

runScenario env goal = case verbosity env of
  Just VCicero      -> toCicero  env goal
  Just VXAlgo       -> toXAlgo   env goal
  Just (VAlgebra x) -> toAlgebra env goal
  Nothing           -> showdouble $ evalMath (facts env) goal
  Just VYAML        -> showdouble $ evalMath (facts env) goal
  _                 -> paragraphTransfer $ case verbosity env of
      Just VAbstract -> "the goal is defined as\n"
      Just VObfuscated -> "the goal, believe it or not, is\n"
      Just VNeutral  -> "the goal you're looking for is\n"
      Just VConcrete -> "showing how we obtain the answer\n"
      _ -> ""
    <> (sayWithAnswer 1 env goal)

type LangID = Text.Text   -- "en-us", "en-gb"
type NLKey = Text.Text    -- "potato" -- language-independent
type NLSyntax = Text.Text -- in future this will be some kind of GF structure handling parts of speech, pluralization, gender, etc.
type NLdict = Map.Map LangID (Map.Map NLKey NLAttr)

data NLAttr = NLAttr { nlporv   :: LegislativeParamOrUserVar
                     , nlsyntax :: NLSyntax
                     }
              deriving (Show, Ord, Eq)

data LegislativeParamOrUserVar = LegislativeParam | UserVar | BitOfBoth
              deriving (Show, Ord, Eq)

enlookup :: Text.Text -> NLdict -> Maybe NLAttr
enlookup s md = do
  en_dict <- Map.lookup "en" md
  dict_val <- Map.lookup s en_dict
  return dict_val

loadTestFile :: String -> IO [Text.Text]
loadTestFile filename = do
  file <- readFile filename
  return (Text.pack <$> lines file)

fakeEnv = Env {verbosity = Just VAbstract, facts = Map.fromList [(FS "additional_per_dependant",FD 500.0),(FS "combined_income",FD 20000.0),(FS "dependants",FD 0.0),(FS "income_threshold",FD 25180.0),(FS "initial_contribution",FD 160.0),(FS "maximum_allowable",FD 630.0),(FS "period",FD 2019.0),(FS "rates_total",FD 1000.0)], nldict = Map.fromList [("en",Map.fromList [("additional_per_dependant",NLAttr {nlporv = LegislativeParam, nlsyntax = "the additional allowable income per dependant"}),("combined_income",NLAttr {nlporv = UserVar, nlsyntax = "the ratepayer's income for the preceding tax year"}),("dependants",NLAttr {nlporv = UserVar, nlsyntax = "person who was a dependant of the ratepayer at the commencement of the rating year in respect of which the application is made"}),("excess_rates_amount",NLAttr {nlporv = BitOfBoth, nlsyntax = "the excess rates amount"}),("income_taper_amount",NLAttr {nlporv = BitOfBoth, nlsyntax = "the income taper amount"}),("income_taper_trigger",NLAttr {nlporv = BitOfBoth, nlsyntax = "the income taper trigger"}),("income_threshold",NLAttr {nlporv = LegislativeParam, nlsyntax = "the income threshold"}),("initial_contribution",NLAttr {nlporv = LegislativeParam, nlsyntax = "the initial contribution by ratepayer"}),("maximum_allowable",NLAttr {nlporv = LegislativeParam, nlsyntax = "the maximum rebate allowed"}),("rates_total",NLAttr {nlporv = UserVar, nlsyntax = "the rates payable for that rating year in respect of the property"})])]}


toCicero :: Environment -> MathExpr Double -> Text.Text
toCicero env goal = Text.unlines $
  [ "/**"
  , " * "
  , " * L4 -> Hyperledger Cicero, for interop with Accord Project"
  , " */"
  , "transaction rates_rebates_" <> (Data.Maybe.fromMaybe "generic" $ unfd $ Map.lookup (FS "period") (facts env)) <> " {"
  ] ++ (
  let nld = (nldict env)
      endict = fromJust $ Map.lookup "en" nld
  in do
    nl <- Map.toList endict
    let (k,v) = nl
        nlp = nlporv v
        nls = nlsyntax v
    return $ "    /** " <> nls <> "  */\n  o Double " <> k)
  ++ (Text.lines $ "    define rebate_amount = " <> toAlgebra (env { verbosity = Just (VAlgebra 0) }) goal) 
  ++ ["}"] 
  where unfd Nothing = Nothing
        unfd (Just (FD y)) = return $ Text.pack $ show $ floor y
        unfd _ = Nothing
  
-- EFFECTIVE
--   IN "CA-NB", "CA-QC"
--   FROM "2018-04-01T00:00"
--   TO "9999-12-30T23:59"
--   TIMEZONE "America/Toronto";

-- META
--   VERSION "0.0.1"
--   RUNTIME "0.4.0"
--   CRITICALITY "experimental"
--   MANAGER "Joseph Potvin <jpotvin@xalgorithms.org>"
--   MAINTAINER "Don Kelly <karfai@gmail.com>";

-- WHEN envelope:type == "invoice";
-- WHEN item:quantity.value > 0;
-- WHEN item:id.value == "a";

-- REQUIRE org.xalgorithms.examples.a_plus_b:all_bs:0.0.1;

-- ASSEMBLE changes
--   COLUMNS FROM table:items
--   COLUMNS FROM table:all_bs;

-- FILTER table:changes
--   WHEN @id.value == @code;

-- MAP table:changes
--   USING new_price = add(@price.value, @b);

-- REVISE table:items
--   UPDATE price.value
--   FROM table:changes;
    
toXAlgo :: Environment -> MathExpr Double -> Text.Text
toXAlgo env goal = Text.unlines
  [ "/**"
  , " * "
  , " * L4 -> XAlgorithms"
  , " * "
  , " */"
  , "EFFECTIVE"
  , "  IN \"NZ\""
  , "  FROM \"2018-04-01T00:00\""
  , "  TO \"9999-12-30T23:59\""
  , "  TIMEZONE \"New Zealand\";"
  , ""
  , "META"
  , "  VERSION \"0.0.1\""
  , "  RUNTIME \"0.1.0\""
  , "  CRITICALITY \"experimental\""
  , "  MANAGER \"Jacinta Ardern <jardern@nz>\""
  , ""
  , "WHEN benefit:type == \"rates_rebate\";"
  , "WHEN payer:rate.value > 160;"
  , "WHEN payer:income.value >= 25180;"
  , ""
  , "REQUIRE org.xalgorithms.nz.rate_rebates_2019:0.0.1;"
  , ""
  , "ASSEMBLE ratepayer_dependants"
  , "  COLUMNS FROM table:dependants"
  , "  COLUMNS FROM table:rate_payer;"
  , ""
  , "ASSEMBLE parameters"
  , "  COLUMNS FROM table:rates"
  , ""
  , "FILTER table:rates"
  , "  WHEN @id.value == @code;"
  , ""
  , "MAP table:changes"
  , "  USING additional_per_dependant = multiply(@dependants.count, 500);"
  , "  USING excess_income = subtract(@ratepayer.income, @rates.income_threshold);"
  , "  USING rates_excess = multiply(2/3, subtract(@payer.rate.value, 160));"
  , "  USING income_allowance = intdiv(excess_income, 8);"
  , ""
  , "REVISE table:rates"
  , "  UPDATE rebate"
  , "  FROM table:changes;"
  , ""
  ]
  
    
