{-# LANGUAGE LambdaCase #-}

module DMN.Translate.JS where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/js-good-parts

import DMN.DecisionTable
import Data.List
import Data.Maybe
import Data.Char
import DMN.Types

data JSOpts = JSOpts { propstyle :: Bool
                     , typescript :: Bool }

toJS :: JSOpts -> DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toJS jsopts dt =
  unlines $ (if propstyle jsopts && typescript jsopts
              then mkArgSpec jsopts (tableName dt) (header dt)
                   <> mkReturnSpec jsopts (tableName dt) (header dt)
              else mempty) ++
             [ unwords $ concat [ mkFunction (tableName dt)
                                , if propstyle jsopts
                                  then mkProps jsopts (tableName dt) (getInputHeaders $ header dt)
                                       ++ ([": " ++ returnName (tableName dt) | typescript jsopts])
                                  else mkArguments jsopts (header dt)
                                , [ "{"]
                                ] ]
             ++ zipWith (\if_ dtrow -> mkIf jsopts (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt)
             ++ [ "}" ]
  where
    elsif = "if" : repeat ( case hitpolicy dt of
                               HP_Unique    -> "else if"
                               HP_First     -> "else if"
                               HP_Priority  -> "else if"
                               HP_Collect _ -> "if"
                               _            -> "if")
  
mkArgSpec :: JSOpts -> String -> [ColHeader] -> [String]
mkArgSpec jsopts tablename chs = mkTypeSpec jsopts (propsName tablename) (getInputHeaders chs)

mkReturnSpec :: JSOpts -> String -> [ColHeader] -> [String]
mkReturnSpec jsopts tablename chs = mkTypeSpec jsopts (returnName tablename) (getOutputHeaders chs)

mkTypeSpec :: JSOpts -> String -> [ColHeader] -> [String]
mkTypeSpec jsopts specname chs =
  ["type " ++ specname ++ " = {"] ++
  ["    \"" ++ varname ch ++ "\" : " ++ maybe "any" type2js (vartype ch) ++ ";" | ch <- chs ] ++
  ["}"]

mkFunction :: String -> [String]
mkFunction tablename = [ "export", "function", underscore tablename ]

mkProps :: JSOpts -> String -> [ColHeader] -> [String]
mkProps jsopts tablename chs = ["(", "props" ++ if typescript jsopts then " : " ++ propsName tablename else "", ")"]

propsName :: String -> String
propsName tablename = "Props_" ++ underscore tablename

returnName :: String -> String
returnName tablename = "Return_" ++ underscore tablename

mkArguments :: JSOpts -> [ColHeader] -> [String]
mkArguments jsopts chs = ["(", intercalate ", " (mkArgument jsopts <$> input_headers chs), ")"]

mkArgument :: JSOpts -> ColHeader -> String
mkArgument jsopts ch = var_name ch ++ maybe "" (if typescript jsopts then (" : " ++) . type2js else const "") (vartype ch)

type2js DMN_String    = "string"
type2js DMN_Number    = "number"
type2js DMN_Boolean   = "boolean"
type2js (DMN_List x)  = type2js x ++ "[]"

mkIf :: JSOpts -> HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf jsopts hp ifword chs dtrow =
  let conditions = uncurry (fexp2js jsopts) <$> catMaybes ( zipWith nonBlankCols (input_headers chs) (row_inputs dtrow) )
  in
    "  " ++ ifword ++ " (" ++
    (if not (null conditions)
     then intercalate " && " conditions
     else "\"default\"") -- TODO: tweak ifword to allow just an "else" here, rather than exploiting the truthiness of JS
    ++ ") { // " ++
    maybe "cont'd" show (row_number dtrow) ++ "\n" ++
    (let feelout = feel2jsOut hp chs dtrow
         standard = maybe "" (\infra -> "    " ++ infra ++ "\n") (fst feelout) ++ "    return {" ++ intercalate ", " (snd feelout) ++ "};"
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
    ++ annotationsAsComments chs dtrow
    ++ "  }"

-- if the row has multiple annotation columns, show the varname of the column header.
-- if there is only one visible annotation column, hide the varname of the column header.
annotationsAsComments :: [ColHeader] -> DTrow -> String
annotationsAsComments chs dtrow =
  let prefixedComments = catMaybes $ zipWith (\cheader commentcol -> ((varname cheader ++ ": ") ++) <$> commentcol) (comment_headers chs) (row_comments dtrow)
      unprefixed = catMaybes $ row_comments dtrow
  in
  unlines $ ("    // "++) <$> (if length unprefixed > 1 then prefixedComments else unprefixed)

fexp2js :: JSOpts -> ColHeader -> [FEELexp] -> String
fexp2js jsopts ch fexps = wrapParen " || " (feel2jsIn ( showVarname jsopts ch) <$> fexps)

showVarname :: JSOpts -> ColHeader -> String
showVarname jsopts ch
  | propstyle jsopts = "props[\"" ++ varname ch ++ "\"]"
  | otherwise        = var_name ch
               
wrapParen myop xs
  | length xs  > 1 = "(" ++ intercalate myop xs ++ ")"
  | length xs == 1 = head xs
  | otherwise      = "null"
wrapArray myop xs = "[" ++ intercalate myop xs ++ "]"

nonBlankCols chs dtrows = if dtrows /= [FAnything] then Just (chs, dtrows) else Nothing

input_headers   = filter ((DTCH_In==).label)
comment_headers = filter ((DTCH_Comment==).label)

feel2jsIn :: String -> FEELexp -> String
feel2jsIn lhs  FAnything = wrapParen "||" ["true",lhs]
feel2jsIn lhs (FSection Feq (VB rhs))  = lhs ++ "===" ++ (toLower <$> show rhs)
feel2jsIn lhs (FSection Feq (VN rhs))  = lhs ++ "===" ++ show rhs
feel2jsIn lhs (FSection Feq (VS rhs))  = lhs ++ "===" ++ show rhs
feel2jsIn lhs (FSection Flt  (VN rhs)) = lhs ++ " < "  ++ show rhs
feel2jsIn lhs (FSection Flte (VN rhs)) = lhs ++ " <="  ++ show rhs
feel2jsIn lhs (FSection Fgt  (VN rhs)) = lhs ++ " > "  ++ show rhs
feel2jsIn lhs (FSection Fgte (VN rhs)) = lhs ++ " >="  ++ show rhs
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
     uncurry showFeels <$> zip
                             (filter ((DTCH_Out==).label) chs)
                             (row_outputs dtrow))
  
type ColPair = (ColHeader, [FEELexp])

showFeels :: ColHeader -> [FEELexp] -> String
showFeels ch fexps = "\"" ++ varname ch ++ "\":" ++ if squash
                                                    then showFeel $ head fexps
                                                    else wrapArray "," (showFeel <$> fexps)
  where squash = maybe True (\case
                                DMN_List _ -> False
                                _          -> True) (vartype ch)
          
showFeel :: FEELexp -> String
showFeel (FNullary (VS str))  = show str
showFeel (FNullary (VN num))  = show num
showFeel (FNullary (VB bool)) = toLower <$> show bool
showFeel (FSection Feq  (VB rhs)) = "(x)=>x === "++ (toLower <$> show rhs)
showFeel (FSection Feq  (VS rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Feq  (VN rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Flt  (VN rhs)) = "(x)=>x < "++show rhs
showFeel (FSection Flte (VN rhs)) = "(x)=>x <="++show rhs
showFeel (FSection Fgt  (VN rhs)) = "(x)=>x > "++show rhs
showFeel (FSection Fgte (VN rhs)) = "(x)=>x >="++show rhs
showFeel (FInRange lower upper)   = "(x)=>" ++ show lower ++ "<= x && x <= " ++ show upper
showFeel (FFunction (FNF1 var))     = var
showFeel (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel (FFunction (FNF0 (VN num)))  = show num
showFeel (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel (FFunction rhs) ++ ")"
showFeel  FAnything               = "undefined"

showFNOp2 :: FNOp2 -> String
showFNOp2 FNMul   = " * "
showFNOp2 FNDiv   = " / "
showFNOp2 FNPlus  = " + "
showFNOp2 FNMinus = " - "
showFNOp2 FNExp   = " ** "
