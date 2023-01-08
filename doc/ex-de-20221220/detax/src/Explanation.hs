{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Explanation where

import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import Data.Tree
import Data.List ( intercalate )
import Data.Ord ()
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)

-- | our Explainable monad supports the evaluation-with-explanation of expressions in our DSL.
-- We make use of Reader, Writer, and State.
type Explainable a = RWST HistoryPath () MyState IO (a,XP)

-- | The Reader supports environmental context for a given evaluation.
-- As we evaluate down from the root to the leaves,
-- - we record the output of previous evaluations in History
-- - we record the current path from the root in Path
type HistoryPath = ([String], [String])

-- | The Writer supports a log of explanations corresponding to the unfolding evaluation tree.
-- The explanation is designed to be readable as an Org-mode file.
-- The "Stdout" component gets rendered within an Example block.
-- The "Stdexp" component gets rendered as the heading followed by whatever body.
-- Actually, we're going to set Writer to () and just stitch together the tree output by hand.
type Stdout = [String]
type Stdexp = [String]
type XP = Tree (Stdout, Stdexp)

-- | The State supports a symbol table in which variables and functions are tracked.
-- We have a couple different symbol tables, one for numeric and one for boolean functions.
type SymTab = Map.Map String
data MyState = MyState { symtabF :: SymTab (Expr Float)
                       , symtabP :: SymTab (Pred Float) }
  deriving (Show, Eq)
-- * Now we do a deepish embedding of an eDSL.

-- | Numeric expressions are things that evaluate to a number.
-- The a here is pretty much always a Fractional here.
data Expr a = Val a
            | Parens (Expr a)
            | MathBin MathBinOp (Expr a) (Expr a)
            | MathVar String
            | MathSum  [Expr a]
            | MathProd [Expr a]
            | MathITE (Pred a) (Expr a) (Expr a)
            | ListFold SomeFold (ExprList a)
            deriving (Eq, Show)

data ExprList a
  = ListFilt Comp (Expr a) (ExprList a)
  | MathList [Expr a]
  deriving (Eq, Show)

(|+),(|-),(|*),(|/) :: Expr Float -> Expr Float -> Expr Float
x |+ y = MathBin Plus   x y
x |- y = MathBin Minus  x y
x |* y = MathBin Times  x y
x |/ y = MathBin Divide x y

data SomeFold = FoldSum | FoldProduct
              deriving (Eq, Show)

-- | fmaps.
-- In Haskell, we would say @(+2) <$> [1,2,3]@
-- Here, we would say @2 +| [1,2,3]@
(+|),(-|),(*|),(/|) :: Expr Float -> [Expr Float] -> [Explainable Float]
x +| ys = map (eval . MathBin Plus   x) ys
x -| ys = map (eval . MathBin Minus  x) ys
x *| ys = map (eval . MathBin Times  x) ys
x /| ys = map (eval . MathBin Divide x) ys

-- | filters.
-- In Haskell, we would say @filter (>0) [-2,-1,0,1,2]
-- Here, we would say @0 <| [-2,-1,0,1,2]@

(<|),(|>) :: Expr Float -> ExprList Float -> ExprList Float
x <| ys = ListFilt CLT x ys
x |> ys = ListFilt CGT x ys

(!|) :: Pred a -> Pred a
(!|) = PredNot

data MathBinOp = Plus | Minus | Times | Divide
  deriving (Eq, Show)

type PredList a = [Pred a]

-- | conditional predicates: things that evaluate to a boolean
data Pred a
  = PredVal Bool
  | PredNot (Pred a)                       -- ^ boolean not
  | PredComp Comp (Expr a) (Expr a)        -- ^ Ord comparisions: x < y
  | PredVar String                         -- ^ boolean variable name
  | PredITE (Pred a) (Pred a) (Pred a)     -- ^ if then else, booleans
  deriving (Eq, Show)

-- | for our notion of Data.Ord
data Comp = CEQ | CGT | CLT | CGTE | CLTE
  deriving (Eq, Show)

-- | variables
data Var a
  = VarMath String (Expr a)
  | VarPred String (Pred a)
    deriving (Eq, Show)

evalP :: Pred Float -> Explainable Bool
evalP (PredVal x) = do
  return (x, Node ([],[show x ++ ": a leaf value"]) [])
evalP (PredNot x) = do
  (xval,xpl) <- local (fmap ("not" :)) (evalP x)
  return (not xval, Node ([] ,[show x ++ ": logical not of"]) [xpl])
evalP (PredComp c x y) =
  let title = "comparison"
  in local (fmap ( title <> " " <> show c :) ) $ do
    (xval, xpl) <- eval x
    (yval, ypl) <- eval y
    let c' = compare xval yval
        toreturn = case c of
          CEQ  | c' == EQ           -> True
          CGT  | c' == GT           -> True
          CLT  | c' == LT           -> True
          CGTE | c' `elem` [GT, EQ] -> True
          CLTE | c' `elem` [LT, EQ] -> True
          _                         -> False
        (lhs,rhs) = verbose title
    return (toreturn, (Node ([]
                            ,[show toreturn ++ ": " ++ lhs ++ " (" ++ show c ++ ")"])
                       [ xpl
                       , mkNod rhs
                       , ypl ]))

evalP (PredVar str) =
  let title = "variable expansion"
      (lhs,rhs) = verbose title
  in local (fmap ((title <> " " <> show str) :)) $ do
    (xvar, xpl1) <- getvarP str
    (xval, xpl2) <- evalP xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])

getvarF :: String -> Explainable (Expr Float)
getvarF x = do
  symtab <- gets symtabF
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])
  
getvarP :: String -> Explainable (Pred Float)
getvarP x = do
  symtab <- gets symtabP
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])
  
pathSpec :: [String] -> String
pathSpec = intercalate " / " . reverse

eval :: Expr Float -> Explainable Float
eval (Val x) = do
  (history,path) <- ask
  return (x, Node ([unlines history ++ pathSpec path ++ ": " ++ show x]
                  ,[show x ++ ": a leaf value"]) [])
eval (MathBin Plus   x y) = binEval "addition"       (+) x y
eval (MathBin Minus  x y) = binEval "subtraction"    (-) x y
eval (MathBin Times  x y) = binEval "multiplication" (*) x y
eval (MathBin Divide x y) = binEval "division"       (/) x y
eval (Parens x)           = unaEval "parentheses"    id  x
eval (ListFold FoldSum     xs) = doFold "sum" sum xs
eval (ListFold FoldProduct xs) = doFold "product" product xs                              

doFold :: String -> ([Float] -> Float) -> ExprList Float -> Explainable Float
doFold str f xs = local (fmap ("listfold " <> str :)) $ do
  (MathList yvals,yexps) <- evalList xs
  zs <- mapM eval yvals
  return (f (fst <$> zs)
         , Node ([],[": " ++ str ++ " of " ++ show (length zs) ++ " elements"])
           (yexps : (snd <$> zs)))
  
evalList :: ExprList Float -> Explainable (ExprList Float)
evalList (MathList a) = return (MathList a, Node (show <$> a,["base MathList"]) [])
evalList (ListFilt comp x lf2@(ListFilt{})) = do
  (lf2val, lf2xpl) <- evalList lf2
  (lf3val, lf3xpl) <- evalList (ListFilt comp x lf2val)
  return (lf3val, Node ([],["recursing RHS ListFilt"]) [lf2xpl, mkNod "becomes", lf3xpl])
evalList (ListFilt comp x (MathList ys)) = do
  round1 <- mapM (evalP . PredComp comp x) ys
  let round2 = [ if r1
                 then (Nothing, Node ([show xval]
                                     , ["excluded " ++ show xval ++
                                        " due to failed comparison test"]) [xpl])
                 else (Just xval, Node ([show xval]
                                       , ["included " ++ show xval ++
                                          "due to passing comparison test"]) [xpl])
               | ((r1,xpl), xval) <- zip round1 ys
               ]
  return ( MathList (mapMaybe fst round2)
         , Node ([], ["reduced " ++ show (length round1) ++ " to " ++
                       show (length round2) ++ " items in list"])
           $ fmap snd round2)

unaEval :: String -> (Float -> Float) -> Expr Float -> Explainable Float
unaEval title f x =
  let (lhs,_rhs) = verbose title
  in local (fmap (title :)) $ do
    (xval, xpl) <- eval x
    let toreturn = f xval
    return (toreturn, Node ([], [show toreturn ++ ": " ++ lhs]) [xpl])

mkNod :: a -> Tree ([b],[a])
mkNod x = Node ([],[x]) []
  
binEval :: String -> (Float -> Float -> Float) -> Expr Float -> Expr Float -> Explainable Float
binEval title f x y = local (fmap (title :)) $ do
  -- liftIO putStrLn should be treated as more of a Debug.Trace.
  -- "normal" output gets returned in the fst part of the Node.
  -- normal output then gets output inside a #+begin_example/#+end_example block.
  -- liftIO $ putStrLn $ "eval " ++ title ++ ": path is " ++ intercalate " / " (reverse path)
  (xval, xpl) <- eval x
  (yval, ypl) <- local (\(h,p) -> (h ++ [show xval],p)) (eval y)
   -- we sneak in monadic history of the upper evaluations
  let toreturn = f xval yval
      (lhs,rhs) = verbose title
  return (toreturn, Node (fst (rootLabel xpl) ++ fst (rootLabel ypl)
                         , [show toreturn ++ ": " ++ lhs])
                    [xpl, mkNod rhs, ypl] )

verbose :: String -> (String, String)
verbose "addition"       = ("which we obtain by adding", "to")
verbose "subtraction"    = ("which we obtain by taking", "minus")
verbose "division"       = ("which we obtain by dividing", "by")
verbose "multiplication" = ("which we obtain by multiplying", "by")
verbose "parentheses"    = ("which is a parenthesized", "")
verbose "negation"       = ("which is logical negation of", "")
verbose "comparison"     = ("which is the result of comparing", "with")
verbose "variable expansion" = ("which comes from the variable", "")
verbose x                = (x, x ++ " argument")

toplevel :: IO ()
toplevel = forM_ [ Val 2 |+ (Val 5 |- Val 1)
                 , ListFold FoldSum $ Val 2 <| MathList [Val 1, Val 2, Val 3, Val 4]
                 ] $ \topexpr -> do
  ((val,xpl), stab, _) <- runRWST
                      (eval topexpr)
                      ([],["toplevel"])             -- reader: HistoryPath
                      (MyState Map.empty Map.empty) -- state: MyState

  putStrLn $ "* toplevel: val = "    ++ show val
  putStrLn $ "* toplevel: symtab = " ++ show stab
  putStrLn $ "* toplevel: xpl = " ++ show val ++ "\n" ++ drawTreeOrg 2 xpl

drawTreeOrg :: Int -> XP -> String
drawTreeOrg depth (Node (stdout, stdexp) xs) =
  unlines ( (replicate depth '*' ++ " " ++ unlines stdexp)
            : [ "#+begin_example\n" ++ unlines stdout ++ "#+end_example" | not (null stdout) ] )
  ++
  unlines ( drawTreeOrg (depth + 1) <$> xs )
  

