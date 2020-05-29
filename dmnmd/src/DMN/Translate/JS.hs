{-# LANGUAGE LambdaCase #-}

module DMN.Translate.JS where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/js-good-parts

import DMN.DecisionTable
import Data.List
import Data.Maybe
import Data.Char
import DMN.Types

toJS :: DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toJS dt = unlines $ ([
  unwords $ concat [ mkFunction (tableName dt)
                     , mkArguments (header dt)
                     , ["{"]
                     ] ]
  ++ (zipWith (\if_ dtrow -> mkIf (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt))
  ++ [ "}" ])
  where
    elsif = "if" : (repeat $ case (hitpolicy dt) of
                               HP_Unique    -> "else if"
                               HP_First     -> "else if"
                               HP_Priority  -> "else if"
                               HP_Collect _ -> "if"
                               _            -> "if")

          
mkFunction :: String -> [String]
mkFunction tablename = [ "export", "function", tablename ]

mkArguments :: [ColHeader] -> [String]
mkArguments chs = ["(", (intercalate ", " (mkArgument <$> input_headers chs)), ")"]

mkArgument :: ColHeader -> String
mkArgument ch = varname ch ++ maybe "" ((" : " ++) . type2js) (vartype ch)

type2js DMN_String    = "string"
type2js DMN_Number    = "number"
type2js DMN_Boolean   = "boolean"
type2js (DMN_List x)  = type2js x ++ "[]"

mkIf :: HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf hp ifword chs dtrow =
  let conditions = ((uncurry fexp2js) <$> (catMaybes $ (zipWith nonBlankCols (input_headers chs) (row_inputs dtrow))))
  in
    "  " ++ ifword ++ " (" ++
    (if length conditions > 0
     then (intercalate " && " conditions)
     else "\"default\"") -- TODO: tweak ifword to allow just an "else" here, rather than exploiting the truthiness of JS
    ++ ") { // " ++
    (fromMaybe "cont'd" (show <$> row_number dtrow)) ++ "\n" ++
    (let feelout = feel2jsOut hp chs dtrow
         standard = (maybe "" (\infra -> "    " ++ infra ++ "\n") (fst feelout)) ++ "    return {" ++ (intercalate ", " (snd feelout)) ++ "};"
     in
     case hp of
       HP_Unique    -> standard
       HP_Any       -> standard
       HP_Collect _ -> standard
       HP_First     -> standard
       HP_Priority  -> standard
       HP_OutputOrder -> standard
       HP_RuleOrder -> standard
       HP_Aggregate -> standard
    )
    ++ "\n"
    ++ (annotationsAsComments chs dtrow)
    ++ "  }"

-- if the row has multiple annotation columns, show the varname of the column header.
-- if there is only one visible annotation column, hide the varname of the column header.
annotationsAsComments :: [ColHeader] -> DTrow -> String
annotationsAsComments chs dtrow =
  let prefixedComments = catMaybes $ (zipWith (\cheader commentcol -> ((varname cheader ++ ": ") ++) <$> commentcol) (comment_headers chs) (row_comments dtrow))
      unprefixed = catMaybes $ row_comments dtrow
  in
  (unlines $ ("    // "++) <$> (if (length unprefixed > 1) then prefixedComments else unprefixed))

fexp2js :: ColHeader -> [FEELexp] -> String
fexp2js ch fexps = wrapParen " || " ((feel2jsIn $ varname ch) <$> fexps)

wrapParen myop xs = if (length xs > 1) then "(" ++ (intercalate myop xs) ++ ")" else if length xs == 1 then xs!!0 else "null"
wrapArray myop xs =                         "[" ++ (intercalate myop xs) ++ "]"

nonBlankCols chs dtrows = if dtrows /= [FAnything] then Just (chs, dtrows) else Nothing

input_headers   = filter ((DTCH_In==).label)
comment_headers = filter ((DTCH_Comment==).label)

feel2jsIn :: String -> FEELexp -> String
feel2jsIn lhs (FAnything) = wrapParen "||" ["true",lhs]
feel2jsIn lhs (FSection Feq (VB rhs))  = (lhs ++ "===" ++ (toLower <$> show rhs))
feel2jsIn lhs (FSection Feq (VN rhs))  = (lhs ++ "===" ++ show rhs)
feel2jsIn lhs (FSection Feq (VS rhs))  = (lhs ++ "===" ++ show rhs)
feel2jsIn lhs (FSection Flt  (VN rhs)) = (lhs ++ " < "  ++ show rhs)
feel2jsIn lhs (FSection Flte (VN rhs)) = (lhs ++ " <="  ++ show rhs)
feel2jsIn lhs (FSection Fgt  (VN rhs)) = (lhs ++ " > "  ++ show rhs)
feel2jsIn lhs (FSection Fgte (VN rhs)) = (lhs ++ " >="  ++ show rhs)
feel2jsIn lhs (FInRange lower upper)   = wrapParen " && " [show lower ++ "<=" ++ lhs, lhs ++ "<=" ++ show upper]
feel2jsIn lhs (FNullary rhs)           = feel2jsIn lhs (FSection Feq rhs)

-- TODO:
-- let's extend FEEL with support for PCRE lol

-- if there's a single output column then we just return that value
-- if there are multiple output columns then we construct an object with multiple properties and return the object.

-- we could treat each output column as a lambda with access to the input namespace.
-- if there's only one input column, then we honour sections by returning the boolean result of operating against the unnamed input
-- if there are multiple input columns then we require explicit use of the input column varname

feel2jsOut :: HitPolicy -> [ColHeader] -> DTrow -> (Maybe String, [String])
feel2jsOut hp chs dtrow
  -- ("// one input column, allowing binary operators in output column(s)"
  = (Nothing, -- "toreturn[thiscolumn] = ..."
     uncurry showFeels <$> (zipWith (,)
                             (filter ((DTCH_Out==).label) chs)
                             (row_outputs dtrow)))
  
type ColPair = (ColHeader, [FEELexp])

showFeels :: ColHeader -> [FEELexp] -> String
showFeels ch fexps = "\"" ++ (varname ch) ++ "\":" ++ (if squash
                                                        then showFeel $ head fexps
                                                        else (wrapArray "," (showFeel <$> fexps)))
  where squash = maybe True (\case
                                DMN_List _ -> False
                                _          -> True) (vartype ch)
          
showFeel :: FEELexp -> String
showFeel (FNullary (VS str))  = show str
showFeel (FNullary (VN num))  = show num
showFeel (FNullary (VB bool)) = show bool
showFeel (FSection Feq  (VB rhs)) = "(x)=>x === "++ (toLower <$> show rhs)
showFeel (FSection Feq  (VS rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Feq  (VN rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Flt  (VN rhs)) = "(x)=>x < "++show rhs
showFeel (FSection Flte (VN rhs)) = "(x)=>x <="++show rhs
showFeel (FSection Fgt  (VN rhs)) = "(x)=>x > "++show rhs
showFeel (FSection Fgte (VN rhs)) = "(x)=>x >="++show rhs
showFeel (FInRange lower upper)   = "(x)=>" ++ (show lower) ++ "<= x && x <= " ++ (show upper)
showFeel (FFunction (FNF1 var))     = var
showFeel (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel (FFunction (FNF0 (VN num)))  = show num
showFeel (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel (FFunction rhs) ++ ")"
showFeel (FAnything)              = "undefined"

showFNOp2 :: FNOp2 -> String
showFNOp2 FNMul   = " * "
showFNOp2 FNDiv   = " / "
showFNOp2 FNPlus  = " + "
showFNOp2 FNMinus = " - "
showFNOp2 FNExp   = " ** "
