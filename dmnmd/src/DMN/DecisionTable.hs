{-# LANGUAGE LambdaCase #-}

module DMN.DecisionTable where

import Control.Arrow
import Prelude hiding (takeWhile)
import DMN.ParseFEEL
import Data.List (filter, dropWhileEnd, transpose, nub, sortOn, sortBy, elemIndex, elem, intersect, isPrefixOf, isSuffixOf, find)
import Data.List.Split
import Data.Maybe
import Data.Either
import Text.Regex.PCRE
import Data.Char (toLower)
import Data.Ord (Ordering(EQ, LT, GT))
import Debug.Trace
import DMN.Types
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

-- main = do
--     putStrLn $ show example1_dish
--     putStrLn $ show $ evalTable example1_dish [VS "Fall"]

evalTable :: DecisionTable -> [FEELexp] -> Either String [[[FEELexp]]]
evalTable table given_input =
  let symtab = Map.fromList $ zip (varname <$> filter ((DTCH_In==).label) (header table)) given_input
      matched = filter ((given_input `matches`) . row_inputs) (datarows table)
      -- evaluate any FFunctions
      outputs = (\row -> row { row_outputs = evalFunctions symtab <$> (row_outputs row) }) <$> matched
  in case hitpolicy table of
    HP_Unique -> case length outputs of
                   0 -> Left "no rows returned -- a unique table should have one result!"
                   1 -> Right (row_outputs <$> outputs)
                   _ -> Left $ "multiple rows returned -- this was supposed to be a unique table!\n" ++ (show outputs)
    HP_Any    -> case length outputs of
                   0 -> Left "no rows returned"
                   _ -> if (length (nub (row_outputs <$> outputs)) > 1)
                        then Left ("multiple distinct rows returned -- an Any lookup may return multiple matches but they should all be the same!\n" ++ (show outputs))
                        else Right (row_outputs <$> outputs)
    HP_Priority    -> Right [(row_outputs $ head $ (outputOrder (header table) outputs))]
    HP_First       -> Right [(row_outputs $ head outputs)]
    HP_OutputOrder -> Right (row_outputs <$> (outputOrder (header table) outputs)) -- order according to enums in subheaders.
    HP_RuleOrder   -> Right (row_outputs <$> sortOn row_number outputs)
    HP_Collect Collect_All -> trace ("outputs has length " ++ (show $ length outputs)) $ Right (row_outputs <$> outputs)
    HP_Collect Collect_Cnt -> trace ("outputs has length " ++ (show $ length outputs)) $ Right [[[FNullary (VN (fromIntegral (length outputs) :: Float))]]]
    HP_Collect Collect_Min -> Right [[[FNullary (VN ((minimum $ [ x | (FNullary (VN x)) <- (concat $ concat (row_outputs <$> outputs)) ])))]]]
    HP_Collect Collect_Max -> Right [[[FNullary (VN ((maximum $ [ x | (FNullary (VN x)) <- (concat $ concat (row_outputs <$> outputs)) ])))]]]
    HP_Collect Collect_Sum -> Right [[[FNullary (VN ((    sum $ [ x | (FNullary (VN x)) <- (concat $ concat (row_outputs <$> outputs)) ])))]]]
    _ -> Left ("don't know how to evaluate hit policy " ++ show (hitpolicy table))
  where
    evalFunctions :: SymbolTable -> [FEELexp] -> [FEELexp]
    evalFunctions symtab cells = do
      fexp <- cells
      case fexp of
        FFunction f -> return $ FNullary (fNEval symtab f)
        x           -> return x

outputOrder :: [ColHeader] -> [DTrow] -> [DTrow]
outputOrder chs dtrows =
  sortBy (mySort chs) dtrows
  -- we want to sort the dtrows by the enums left to right
  -- for instance, each DTrow in [DTrow] contain row_outputs; each [FEELexp] in row_outputs belongs to a column ColHeader which has an enums :: Maybe [FEELexp]; that [FEELexp] list is sorted.
  -- we know that the [FEELexp] in row_outputs contains 0 or 1 FEELexps.
  -- so we need to compute, for use by sortOn, a map from each value of the nums [FEELexp], to its elemIndex; and if two elements have the same index, we recursively sort using the next column, until we are able to return an Ordering LT EQ GT
  -- so, we want to sort the list of DTrows on 


mySort :: [ColHeader] -> DTrow -> DTrow -> Ordering
mySort colhs rowa rowb =
  let colenums = enums <$> filter ((DTCH_Out==).label) colhs
  in myS colenums (row_outputs rowa) (row_outputs rowb)
  -- return a function that cursors through all the output rows sorting the FEELexps by the enums in the respective ColHeaders until able to return LT EQ GT

myS :: (Eq a, Show a) => [Maybe [a]] -> [[a]] -> [[a]] -> Ordering
myS orders as bs = -- trace ("myS: will compare " ++ (show as) ++ " with " ++ (show bs))
                   (firstNonEQ "  ") $ zipWith3 sortCol orders as bs

sortCol :: (Eq a, Show a) => Maybe [a] -> [a] -> [a] -> Ordering
sortCol colenum cola colb = -- trace ("sortCol: comparing the elements within " ++ (show cola) ++ " and " ++ (show colb))
                            maybe EQ (\enumlist -> (firstNonEQ "    ") $ zipWith (sortCell enumlist) cola colb) colenum
sortCell :: (Eq a, Show a) => [a] -> a -> a -> Ordering
sortCell cellenums a b = -- trace ("  sortCell: comparing the locations of elements " ++ (show a) ++ " and " ++ (show b) ++ " in " ++ (show cellenums))
                         compare (elemIndex a cellenums) (elemIndex b cellenums)
firstNonEQ :: String -> [Ordering] -> Ordering
firstNonEQ spaces args = let remaining = dropWhile (EQ==) args
                         in -- trace (spaces ++ "firstNonEQ: returning first non-EQ element of " ++ (show args))
                            (if (length remaining == 0) then EQ else head remaining)


matches :: [FEELexp] -> [[FEELexp]] -> Bool
-- matches tableInput testInput = 
matches inpts tableInputs = all (==True) $ zipWith fEvals inpts tableInputs


fe2dval :: FEELexp -> DMNVal
fe2dval (FNullary dmnval) = dmnval
fe2dval fexp = error ("fe2dval can't extract a DMNVal from " ++ show fexp)

-- static analysis phase, input validation of table, should identify scenarios where the variable name does not exist in the input props.
-- feel like we should convert this to a ReaderT so the symtab gets hidden?
fNEval :: SymbolTable -> FNumFunction -> DMNVal
fNEval symtab (FNF0 dmnval) = dmnval
fNEval symtab (FNF1 varname) = fe2dval $ fromMaybe (error $ "function unable to resolve variable " ++ varname) $ Map.lookup varname symtab
fNEval symtab (FNF3 fnf1 fnop2 fnf3) = let lhs = (fromVN (fNEval symtab fnf1))
                                           rhs = (fromVN (fNEval symtab fnf3))
                                           result = case fnop2 of
                                             FNMul -> lhs * rhs
                                             FNDiv -> lhs / rhs
                                             FNPlus -> lhs + rhs
                                             FNMinus -> lhs + rhs
                                             FNExp -> lhs ** rhs
                                       in VN result

mkFs :: (Maybe DMNType) -> String -> [FEELexp]
mkFs dmntype args = do
  arg <- trim <$> splitOn "," args
  return (mkF dmntype arg)


-- TODO: add a state monad to allow type inference to span all input rows;
-- if any row contains a string, that entire column becomes a string not a num;
-- if all the columns contain nums or bools, then they're that;
-- but we can only make that decision after viewing the entire table.
-- maybe we use a multi-pass strategy ... where we allow the cells to remain untyped ... and then we review the entire table
-- after it's been fully parsed once.
mkF :: (Maybe DMNType) -> String -> FEELexp
mkF _ ""  = FAnything
mkF _ "_" = FAnything
mkF _ "-" = FAnything
-- in a numeric column, an FFunction is detected by the presence of an numeric operator
mkF t@(Just (DMN_List _)) x  = mkF (baseType t) x
mkF (Nothing)  arg1 = -- trace ("mkF Nothing shouldn't happen -- type inference should have found some type for this column. coercing to string: " ++ arg1)
  (FNullary (VS (trim arg1)))
mkF (Just DMN_String)  arg1 = FNullary (VS (trim arg1))
mkF (Just DMN_Boolean) arg1 = FNullary (mkVB arg1)
  where
    mkVB arg
      | (toLower <$> arg) `elem` ["true","yes","positive"] = VB True
      | (toLower <$> arg) `elem` ["false","no","negative"] = VB False
      | otherwise                                          = error $  "unable to parse an alleged boolean: " ++ arg
mkF (Just DMN_Number)  arg1
  | length ("+-*/" `intersect` arg2) > 0 = either (error $ "error: parsing suspected function expression " ++ arg2) FFunction (parseOnly parseFNumFunction (T.pack arg2))
  | "<=" `isPrefixOf` arg2 = FSection Flte (mkVN $ trim $ drop 2 arg2)
  | "<"  `isPrefixOf` arg2 = FSection Flt  (mkVN $ trim $ drop 1 arg2)
  | ">=" `isPrefixOf` arg2 = FSection Fgte (mkVN $ trim $ drop 2 arg2)
  | ">"  `isPrefixOf` arg2 = FSection Fgt  (mkVN $ trim $ drop 1 arg2)
  | "<=" `isSuffixOf` arg2 = FSection Fgt  (mkVN $ trim $ (Prelude.take (length arg2 - 2) arg2))
  | "<"  `isSuffixOf` arg2 = FSection Fgte (mkVN $ trim $ (Prelude.take (length arg2 - 1) arg2))
  | ">=" `isSuffixOf` arg2 = FSection Flt  (mkVN $ trim $ (Prelude.take (length arg2 - 2) arg2))
  | arg2 =~ "\\[\\s*(\\d+)\\s*\\.\\.\\s*(\\d+)\\s*\\]" :: Bool =
    let (_,_,_,bounds) = arg2 =~ "\\[\\s*(\\d+)\\s*\\.\\.\\s*(\\d+)\\s*\\]" :: (String,String,String,[String])
    in FInRange ((read $ bounds!!0) :: Float) ((read $ bounds!!1) :: Float)
  | "="  `isPrefixOf` arg2 = FSection Feq  (mkVN $ (trim $ dropWhile    (=='=') arg2))
  | "="  `isSuffixOf` arg2 = FSection Feq  (mkVN $ (trim $ dropWhileEnd (=='=') arg2))
  | otherwise   = FNullary      (mkVN $ (trim                        arg2))
  where arg2 = trim arg1 -- probably extraneous
        mkVN x = VN (read x :: Float)

fromVN :: DMNVal -> Float
fromVN (VN n) = n
fromVN (VB True) = 1.0
fromVN (VB False) = 0.0
fromVN _ = error ("type error: tried to read a float out of a string")

trim :: String -> String
trim = dropWhile (==' ') . dropWhileEnd (==' ')
trimLeft :: String -> String
trimLeft = dropWhile (==' ')
trimRight :: String -> String
trimRight = dropWhileEnd (==' ')
 


fEvals :: FEELexp -> [FEELexp] -> Bool
fEvals arg exps = any (==True) $ (\x -> fEval x arg) <$> exps

-- TODO: recognize ? as a placeholder for the current input value, in case the FEEL expression is more complex.
-- reinventing Ord and Eq typeclasses here. we also deal with "< x" vs "x <" -- we canonicalize order to "op val"
-- column in table -> input parameter -> is there a match?
fEval :: FEELexp -> FEELexp -> Bool
fEval (FAnything)    _                  = True
  -- alternative phrasing without arrows: (snd . fromJust . (find ((== f) . fst))
fEval (FSection f    (VN rhs)) (FNullary (VN lhs)) = (((find ((== f) <<< fst)) >>> fromJust >>> snd)
                                                      [(Flt,(<)), (Flte,(<=)), (Fgt,(>)), (Fgte,(>=)), (Feq,(==))])
                                                     lhs rhs
fEval (FInRange lower upper)   (FNullary (VN lhs)) = lower <= lhs && lhs <= upper
fEval (FSection Feq  (VB rhs)) (FNullary (VB lhs)) = lhs == rhs
fEval (FSection Feq  (VS rhs)) (FNullary (VS lhs)) = lhs == rhs
fEval (FNullary (VS rhs)) (FNullary (VS lhs)) = lhs == rhs
fEval (FNullary (VB rhs)) (FNullary (VB lhs)) = lhs == rhs
fEval (FNullary (VN rhs)) (FNullary (VN lhs)) = lhs == rhs
fEval rhs lhs                                 = error $ unwords $ [ "type error in ", show lhs, " ~ ", show rhs]


-- From the S-FEEL specification:
-- Given an expression o to be tested and two endpoint e1 and e2:
--  is in the interval (e1..e2), also notated ]e1..e2[, if and only if o > e1 and o < e1
--  is in the interval (e1..e2], also notated ]e1..e2], if and only if o > e1 and o ≤ e2
--  is in the interval [e1..e2] if and only if o ≥ e1 and o ≤ e2
--  is in the interval [e1..e2), also notated [e1..e2[, if and only if o ≥ e1 and o < e2
-- An expression to be tested satisfies an instance of simple unary tests (grammar rule 12) if and only if, either the
-- expression is a list and the expression satisfies at least one simple unitary test in the list, or the simple unitary tests is “-”.
-- 


-- perform type inference to resolve colheader values based on a review of the rows
mkDTable :: String -> HitPolicy -> [ColHeader] -> [DTrow] -> DecisionTable
mkDTable origname orighp origchs origdtrows =
--  Debug.Trace.trace ("mkDTable: starting; origchs = " ++ show origchs) $
  let newchs   = zipWith inferTypes (getInputHeaders origchs ++ getOutputHeaders origchs)
                                     (transpose $ [ row_inputs r ++  row_outputs r | r@(DTrow _ _ _ _) <- origdtrows])
      typedchs = (if length newchs > 0 then (newchs ++ getCommentHeaders origchs) else origchs)
  in -- Debug.Trace.trace ("mkDTable: finishing...\n" ++
        --                 "origchs = " ++ show(origchs) ++ "\n" ++
           --             "newchs = " ++ show(newchs) ++ "\n" )
    (DTable origname orighp typedchs
      ((\case
           (DThr) -> DThr
           (DTrow rn ri ro rc) -> (DTrow rn
                                    (reprocessRows (getInputHeaders typedchs)  ri)
                                    (reprocessRows (getOutputHeaders typedchs) ro)
                                    rc)) <$> origdtrows))
                         
reprocessRows :: [ColHeader] -> [[FEELexp]] -> [[FEELexp]]
reprocessRows = 
  -- bang through all columns where the header vartype is Just something, and if the body is FNullary VS, then re- mkF it using the new type info
  zipWith (\ch cells ->
             -- Debug.Trace.trace ("** reprocessRows: have the option to reprocess cells to " ++ show (vartype ch) ++ ": " ++ show cells) $
               if not( vartype ch `elem` [Nothing, Just DMN_String]) && (length [ x | FNullary (VS x) <- cells] == length cells)
               then -- Debug.Trace.trace ("reprocessing to " ++ show (vartype ch) ++ ": " ++ show cells) $
                    [ mkF (vartype ch) x | FNullary (VS x) <- cells ]
               else cells)
                         
  
getInputHeaders :: [ColHeader] -> [ColHeader]
getInputHeaders = getWantedHeaders DTCH_In

getOutputHeaders :: [ColHeader] -> [ColHeader]
getOutputHeaders = getWantedHeaders DTCH_Out

getCommentHeaders :: [ColHeader] -> [ColHeader]
getCommentHeaders = getWantedHeaders DTCH_Comment

getWantedHeaders :: DTCH_Label -> [ColHeader] -> [ColHeader]
getWantedHeaders wantedLabel = filter ((wantedLabel==).label)

inferTypes :: ColHeader   -- in or out header column
           -> [[FEELexp]] -- body column of expressions corresponding to that column
           -> ColHeader   -- revised header column with vartype set
inferTypes origch origrows = -- Debug.Trace.trace ("  infertypes: called with colheader = " ++ show origch ++ "\n           and rows = " ++ show origrows) $
  let coltypes = nub $ catMaybes $ do
        cells <- origrows
        cell <- cells
        return $ (inferType $ cell)

  in if length coltypes == 1
     then let coltype = head coltypes
          in if (vartype origch == Nothing)
             then origch { vartype = Just coltype }
             else if (vartype origch /= Just coltype)
                  then -- Debug.Trace.trace ("    vartype for " ++ (varname origch) ++ " is " ++ (show $ vartype origch) ++ "; but inferred type is " ++ (show coltype))
                       origch
                  else origch { vartype = Just coltype }
     else -- Debug.Trace.trace ("    vartype for " ++ (varname origch) ++ " is " ++ (show $ vartype origch) ++ "; but inferred types are " ++ (show coltypes))
          origch

-- initially, we let type inference work for everything except functions.
-- in the future we may need to change the return type from Maybe DMNType to FEELexp (FNumFunction | FNullary)
inferType :: FEELexp -> Maybe DMNType
inferType (FFunction _) = Just DMN_Number
inferType (FSection _ (VN _)) = Just DMN_Number
inferType (FSection _ (VB _)) = Just DMN_Boolean
inferType (FSection _ (VS _)) = Just DMN_String
inferType (FInRange _ _) = Just DMN_Number
inferType (FAnything) = Nothing
inferType (FNullary (VN _)) = Just DMN_Number
inferType (FNullary (VB _)) = Just DMN_Boolean
inferType (FNullary (VS arg))
  | any (arg =~) ["^\\d+(\\.\\d+)?$", "\\.\\.", ">", "<", "="] = Just DMN_Number
  | arg `elem` ["-","_",""] = Nothing
  | (toLower <$> arg) `elem` ["true","yes","positive","y","false","no","negative","n"] = Just DMN_Boolean
  | head arg == '\"' && last arg == '\"' = Just DMN_String
  | any (arg =~) [" \\* ", " \\+ ", " - ", " / ", " \\*\\* "] = Just DMN_Number
  | otherwise = -- Debug.Trace.trace ("inferType " ++ show arg ++ " returning String!") $
      (Just DMN_String)
