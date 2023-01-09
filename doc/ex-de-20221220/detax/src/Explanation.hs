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
import Data.Bifunctor (first)

-- | our Explainable monad supports the evaluation-with-explanation of expressions in our DSL.
-- We make use of Reader, Writer, and State.
type Explainable r a = RWST         (HistoryPath,r) [String] MyState IO (a,XP)

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
            | MathITE (Pred a) (Expr a) (Expr a)
            | MathMax (Expr a) (Expr a)
            | MathMin (Expr a) (Expr a)
            | ListFold SomeFold (ExprList a)
            deriving (Eq, Show)

(|+),(|-),(|*),(|/) :: Expr Float -> Expr Float -> Expr Float
x |+ y = MathBin Plus   x y
x |- y = MathBin Minus  x y
x |* y = MathBin Times  x y
x |/ y = MathBin Divide x y

data MathSection a
  = Id
  | MathSection MathBinOp (Expr a)
  deriving (Eq, Show)

data SomeFold = FoldSum | FoldProduct | FoldMax | FoldMin
              deriving (Eq, Show)

-- | fmaps.
-- In Haskell, we would say @(+2) <$> [1,2,3]@
-- Here, we would say @2 +| [1,2,3]@
(+|),(-|),(*|),(/|) :: Expr Float -> [Expr Float] -> [Explainable r Float]
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

shw :: Comp -> String
shw CEQ = "=="
shw CGT = ">"
shw CGTE = ">="
shw CLT = "<"
shw CLTE = "<="

-- | variables
data Var a
  = VarMath String (Expr a)
  | VarPred String (Pred a)
    deriving (Eq, Show)

retitle :: String -> Explainable r a -> Explainable r a
retitle st = local (first (fmap (st:))) -- prepend some string to the path part of ((history,path),r)

evalP :: Pred Float -> Explainable r Bool
evalP (PredVal x) = do
  return (x, Node ([],[show x ++ ": a leaf value"]) [])
evalP (PredNot x) = do
  (xval,xpl) <- retitle "not" (evalP x)
  return (not xval, Node ([] ,[show x ++ ": logical not of"]) [xpl])
evalP (PredComp c x y) =
  let title = "comparison"
  in retitle (title <> " " <> shw c) $ do
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
                            ,[show toreturn ++ " " ++ lhs ++ " (" ++ shw c ++ ")"])
                       [ xpl
                       , mkNod rhs
                       , ypl ]))

evalP (PredVar str) =
  let title = "variable expansion"
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarP str
    (xval, xpl2) <- evalP xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])

evalP (PredITE p x y) = evalFP evalP p x y

evalFP :: Show t
       => (t -> Explainable r a)
       -> Pred Float
       -> t
       -> t
       -> Explainable r a
evalFP evf p x y = retitle "if-then-else" $ do
  (pval,pxpl) <- evalP p
  if pval
    then do
    (xval,xxpl) <- evf x
    return (xval, Node ([],["if " ++ show p ++ " then " ++ show x ++ " else " ++ show y] ) [pxpl, mkNod "thus we choose", xxpl])
    else do
    (yval,yxpl) <- evf y
    return (yval, Node ([],["if-then-else false"] ) [pxpl, mkNod "thus we choose", yxpl])



getvarF :: String -> Explainable r (Expr Float)
getvarF x = do
  symtab <- gets symtabF
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])
  
getvarP :: String -> Explainable r (Pred Float)
getvarP x = do
  symtab <- gets symtabP
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])
  
pathSpec :: [String] -> String
pathSpec = intercalate " / " . reverse

historypath :: (hp,r) -> hp
historypath = fst

eval :: Expr Float -> Explainable r Float
eval (Val x) = do
  (history,path) <- asks historypath
  return (x, Node ([unlines history ++ pathSpec path ++ ": " ++ show x]
                  ,[show x ++ ": a leaf value"]) [])
eval (MathBin Plus   x y) = binEval "addition"       (+) x y
eval (MathBin Minus  x y) = binEval "subtraction"    (-) x y
eval (MathBin Times  x y) = binEval "multiplication" (*) x y
eval (MathBin Divide x y) = binEval "division"       (/) x y
eval (Parens x)           = unaEval "parentheses"    id  x
eval (MathITE p x y)      = evalFP eval  p x y
eval (MathMax x y)        = eval (ListFold FoldMax (MathList [x,y]))
eval (MathMin x y)        = eval (ListFold FoldMin (MathList [x,y]))
eval (MathVar str) =
  let title = "variable expansion"
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarF str
    (xval, xpl2) <- eval xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])

eval (ListFold FoldMin     xs) = doFold "min" minimum xs
eval (ListFold FoldMax     xs) = doFold "max" maximum xs
eval (ListFold FoldSum     xs) = doFold "sum" sum xs
eval (ListFold FoldProduct xs) = doFold "product" product xs                              

doFold :: String -> ([Float] -> Float) -> ExprList Float -> Explainable r Float
doFold str f xs = retitle ("listfold " <> str) $ do
  (MathList yvals,yexps) <- evalList xs
  zs <- mapM eval yvals
  let toreturn = f (fst <$> zs)
  return (toreturn
         , Node ([],(show toreturn ++ " = " ++ str ++ " of " ++ show (length zs) ++ " elements")
                  : [ "- " ++ show e | e <- fst <$> zs ])
           (yexps : (snd <$> zs)))
  
data ExprList a
  = MathList [Expr a]
  | ListFilt Comp (Expr a)   (ExprList a)
  | ListMap  (MathSection a) (ExprList a)
  deriving (Eq, Show)

evalList :: ExprList Float -> Explainable r (ExprList Float)
evalList (MathList a) = return (MathList a, Node (show <$> a,["base MathList with " ++ show (length a) ++ " elements"]) [])
evalList (ListFilt comp x lf2@(ListFilt{})) = do
  (lf2val, lf2xpl) <- evalList lf2
  (lf3val, lf3xpl) <- evalList (ListFilt comp x lf2val)
  return (lf3val, Node ([],["recursing RHS ListFilt"]) [lf2xpl, mkNod "becomes", lf3xpl])
evalList (ListFilt comp x (MathList ys)) = do
  origs <- mapM eval ys
  round1 <- mapM (evalP . PredComp comp x) ys
  let round2 = [ if not r1
                 then (Nothing, Node ([show xval]
                                     , ["excluded " ++ show xval ++
                                        " due to failing comparison test"]) [xpl])
                 else (Just xval, Node ([show xval]
                                       , ["included " ++ show xval ++
                                          " due to passing comparison test"]) [xpl])
               | ((r1,xpl), xval) <- zip round1 ys
               ]
      round3 = mapMaybe fst round2
  return ( MathList round3
         , Node ([]
                , (show (length round3) ++ " elements were reduced from an original " ++ show (length round1))
                  : ["- " ++ show (fst o) | o <- origs])
           $ fmap snd round2)
evalList (ListMap Id ylist) = return (ylist, mkNod "id on ExprList")
evalList (ListMap (MathSection binop x) ylist) = retitle "fmap mathsection" $ do
  (MathList ylist', yxpl) <- evalList ylist
  return ( MathList [ MathBin binop x y | y <- ylist' ]
         , Node ([],["fmap mathsection " ++ show binop ++ show x ++ " over " ++ show (length ylist') ++ " elements"]) [yxpl] )


unaEval :: String -> (Float -> Float) -> Expr Float -> Explainable r Float
unaEval title f x =
  let (lhs,_rhs) = verbose title
  in retitle title $ do
    (xval, xpl) <- eval x
    let toreturn = f xval
    return (toreturn, Node ([], [show toreturn ++ ": " ++ lhs]) [xpl])

mkNod :: a -> Tree ([b],[a])
mkNod x = Node ([],[x]) []
  
binEval :: String -> (Float -> Float -> Float) -> Expr Float -> Expr Float -> Explainable r Float
binEval title f x y = retitle title $ do
  -- liftIO putStrLn should be treated as more of a Debug.Trace.
  -- "normal" output gets returned in the fst part of the Node.
  -- normal output then gets output inside a #+begin_example/#+end_example block.
  -- liftIO $ putStrLn $ "eval " ++ title ++ ": path is " ++ intercalate " / " (reverse path)
  (xval, xpl) <- eval x
  (yval, ypl) <- local (\((h,p),r) -> ((h ++ [show xval],p),r)) (eval y)
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
verbose "comparison"     = ("is the result of comparing", "with")
verbose "variable expansion" = ("which comes from the variable", "")
verbose x                = (x, x ++ " argument")

drawTreeOrg :: Int -> XP -> String
drawTreeOrg depth (Node (stdout, stdexp) xs) =
  unlines ( (replicate depth '*' ++ " " ++ unlines stdexp)
            : [ "#+begin_example\n" ++ unlines stdout ++ "#+end_example" | not (null stdout) ] )
  ++
  unlines ( drawTreeOrg (depth + 1) <$> xs )
  

toplevel :: IO ()
toplevel = forM_ [ Val 2 |+ (Val 5 |- Val 1)
                 , ListFold FoldSum $ Val 2 <| MathList [Val 1, Val 2, Val 3, Val 4]
                 , ListFold FoldSum $ Val 0 <| MathList [Val (-2), Val (-1), Val 0, Val 1, Val 2, Val 3]
                 , ListFold FoldSum $ Val 0 <| MathList [Val (-2), Val (-1), Val 0, Val 1, Val 2, Val 3]
                 ] $ \topexpr -> do
  (val, xpl, stab, wlog) <- xplainF () topexpr
  return ()

xplainF :: r -> Expr Float -> IO (Float, XP, MyState, [String])
xplainF r expr = do
  ((val,xpl), stab, wlog) <- runRWST
                             (eval expr)
                             (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                             (MyState Map.empty Map.empty) -- state: MyState
  putStrLn $ "* xplainF"
  putStrLn $ "#+begin_src haskell\n" ++ show expr ++ "\n#+end_src"
  putStrLn $ "** toplevel: val = "    ++ show val
  putStrLn $ "** toplevel: symtab = " ++ show stab
  putStrLn $ "** toplevel: log = "    ++ show wlog
  putStrLn $ "** toplevel: xpl = " ++ show val ++ "\n" ++ drawTreeOrg 3 xpl

  return (val, xpl, stab, wlog)
