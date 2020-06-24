{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module DMN.ParseTable where

import Prelude hiding (takeWhile)
import DMN.DecisionTable
import DMN.ParseFEEL
import Data.Char
import Data.Either
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (filter, dropWhileEnd, transpose)
import Control.Applicative 
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import DMN.Types

pipeSeparator :: Parser ()
pipeSeparator = skipHorizontalSpace >> skip (=='|') >> skipHorizontalSpace

getpipeSeparator :: Parser Text
getpipeSeparator = skipHorizontalSpace *> "|" <* skipHorizontalSpace

parseColHeader :: Parser ColHeader
parseColHeader = do
  mylabel_pre <- parseLabelPre <* skipHorizontalSpace <?> "mylabel_pre"
  myvarname <- parseVarname <?> "parseVarname"
  mytype <- option Nothing parseTypeDecl <?> "parseTypeDecl"
  mylabel_post <- skipHorizontalSpace *> parseLabelPost <?> "parseLabelPost"
  return $ DTCH (mkHeaderLabel mylabel_pre mylabel_post) (T.unpack myvarname) mytype Nothing

parseTypeDecl :: Parser (Maybe DMNType) -- Nothing means it's up to some later code to infer the type. Usually it gets treated just like a String.
parseTypeDecl = do
  skipHorizontalSpace >> ":" >> skipHorizontalSpace
  parseType

parseType :: Parser (Maybe DMNType)
parseType =
  (DMN_List <$>) <$> ("[" *> parseType <* "]" <?> "inside list")
   <|>
   ((("String"  >> return (Just DMN_String)) <?> "string type") <|>
    (("Number"  >> return (Just DMN_Number)) <?> "number type") <|>
    (("Boolean" >> return (Just DMN_Boolean)) <?> "boolean type") ) -- need to check what the official DMN names are for these

mkHeaderLabel (Just "//") _        = DTCH_Comment
mkHeaderLabel (Just "#" ) _        = DTCH_Comment
mkHeaderLabel _ (Just "(comment)") = DTCH_Comment
mkHeaderLabel _ (Just "(out)")     = DTCH_Out
mkHeaderLabel _ (Just "(in)")      = DTCH_In
mkHeaderLabel _  Nothing           = DTCH_In

parseLabelPre  = option Nothing (Just <$> ("//"   <|> "#"))
parseLabelPost = option Nothing (Just <$> ("(in)" <|> "(out)" <|> "(comment)"))

parseHitPolicy :: Parser HitPolicy
parseHitPolicy = 
  mkHitPolicy_  <$> satisfy (inClass "UAPFOR")
  <|>
  (char 'C' >> skipHorizontalSpace >> (mkHitPolicy_C <$> option 'A' (satisfy (inClass "#<>+A"))))

parseHeaderRow :: Parser HeaderRow
parseHeaderRow = do
  pipeSeparator <?> "pipeSeparator"
  myhitpolicy <- parseHitPolicy  <?> "hitPolicy"
  mychs <- many ( pipeSeparator *> parseColHeader <?> "parseColHeader" ) <?> "mychs"
  pipeSeparator <?> "pipeSeparator"
  endOfLine <|> endOfInput
  return $ DTHR myhitpolicy mychs

mkHitPolicy_ :: Char -> HitPolicy
mkHitPolicy_ 'U' = HP_Unique
mkHitPolicy_ 'A' = HP_Any
mkHitPolicy_ 'P' = HP_Priority
mkHitPolicy_ 'F' = HP_First
mkHitPolicy_ 'O' = HP_OutputOrder
mkHitPolicy_ 'R' = HP_RuleOrder

mkHitPolicy_C :: Char -> HitPolicy
mkHitPolicy_C 'A' = HP_Collect Collect_All
mkHitPolicy_C '#' = HP_Collect Collect_Cnt
mkHitPolicy_C '<' = HP_Collect Collect_Min
mkHitPolicy_C '>' = HP_Collect Collect_Max
mkHitPolicy_C '+' = HP_Collect Collect_Sum

-- TODO: switch to Parsec for better error messages

-- TODO: consider allowing spaces in variable names

-- TODO: use sepBy in parsing columns

-- TODO: think about refactoring this into a multi-step parser:
-- 1. read the table into a 2d array of strings
-- 2. intuit whether it's a vertical, horizontal, or crosstab table
-- 3. perform type inference on the table values
-- 4. consider whether the values are consistent with the type declarations in the header
-- 5. vertical and horizontal get canonicalized into a common logical representation
-- 6. crosstab tables get reformatted into that representation
-- 7. then we have a validated decision table.

-- also, see page 112 for boxed expressions -- the contexts are pretty clearly an oop / record paradigm
-- so we probably need to bite the bullet and just support JSON input.

parseTable :: String -> Parser DecisionTable
parseTable tableName = do
  headerRow_1 <- reviseInOut <$> parseHeaderRow <?> "parseHeaderRow"
  let columnSignatures = columnSigs headerRow_1
  subHeadRow <- parseContinuationRows <?> "parseSubHeadRows"
  -- merge headerRow with subHeadRows
  let headerRow = if not (null subHeadRow)
                  then  headerRow_1 { cols = zipWith (\orig subhead -> orig { enums = if subhead == [FAnything] then Nothing else Just subhead } ) -- in the data section a blank cell means anything, but in the subhead it means nothing.
                                             (cols headerRow_1)
                                             (zipWith (\cs cell -> mkFs (snd cs) cell) columnSignatures subHeadRow) }
                  else headerRow_1
  dataRows <- parseDataRows columnSignatures <?> "parseDataRows"
  -- when our type inference is stronger, let's make the cells all just strings, and let the inference engine validate all the cells first, then infer, then construct.
  return ( mkDTable tableName (hrhp headerRow)
           (cols headerRow)
           dataRows )

parseDThr :: Parser DTrow
parseDThr = do
    ("|---" <|> "| -") >> skipWhile (/='\n') >> endOfLine
    return DThr

-- continuation rows are used to batch both subheads and datarow continuations    
parseContinuationRows :: Parser [String]
parseContinuationRows = do
  plainrows <- many (parseContinuationRow <?> "parseContinuationRow")
  return $ trim . unwords <$> transpose plainrows

parseContinuationRow :: Parser [String]
parseContinuationRow = do
      pipeSeparator
      skipHorizontalSpace
      pipeSeparator
      parseTail <?> "parseTail"

parseTail :: Parser [String]
parseTail = do
  rowtail <- manyTill (manyTill (satisfy (/='|')) pipeSeparator) (skipHorizontalSpace >> endOfLine)
  return $ trim <$> rowtail
  
-- the pattern is this:
-- |---|------ DThr --- horizontal rule
-- | N | data row start   A1  | B1 | -- a single logical data row spans multiple physical rows
-- |   | continuation row A2  | B2 | 
-- |   | continuation row A3  | B3 |
-- subsequently, the lines are jammed together and converted to FEEL columns. "A1 A2 A3", "B1 B2 B3" happens thanks to "map unwords $ transpose"


parseDataRows :: [ColumnSignature] -> Parser [DTrow]
parseDataRows csigs = do
  drows <- many ((parseDThr <?> "parseDThr") <|> (parseDataRow csigs <?> "parseDataRow"))
  endOfInput
  return drows

parseDataRow :: [ColumnSignature] -> Parser DTrow
parseDataRow csigs =
  do
      pipeSeparator
      myrownumber <- many1 digit <?> "row number"
      pipeSeparator
      firstrowtail <- parseTail
      morerows <- many parseContinuationRow
      let datacols = zipWith mkFEELCol csigs (map (trim . unwords) $ transpose (firstrowtail : morerows))
      return ( DTrow (if not (null myrownumber) then Just $ (\n -> read n :: Int) myrownumber else Nothing)
               (catMaybes (zipWith getInputs  csigs datacols))
               (catMaybes (zipWith getOutputs csigs datacols))
               (catMaybes (zipWith getComments csigs datacols)) )
  
  -- TODO: if myrownumber is blank, append the current row to the previous row as though connected by a ,
  where
    getInputs (DTCH_In, _) (DTCBFeels fexps) = Just fexps
    getInputs _ _ = Nothing
    getOutputs (DTCH_Out, _) (DTCBFeels fexps) = Just fexps
    getOutputs _ _ = Nothing
    getComments (DTCH_Comment, _) (DTComment mcs) = Just mcs
    getComments _ _ = Nothing

mkFEELCol :: ColumnSignature -> String -> ColBody
mkFEELCol (DTCH_Comment, _)      = mkDataColComment
mkFEELCol (DTCH_In, maybe_type)  = mkDataCol maybe_type
mkFEELCol (DTCH_Out, maybe_type) = mkDataCol maybe_type

type ColumnSignature = (DTCH_Label, Maybe DMNType)

columnSigs :: HeaderRow -> [ColumnSignature]
columnSigs = fmap (\ch -> (label ch, vartype ch)) . cols

-- hack: convert (in, in, in) to (in, in, out)
-- Control.Arrow would probably let us phrase this more cleverly, a la hxt, with "guard" and "when" but this is probably more readable for a beginner
reviseInOut :: HeaderRow -> HeaderRow
reviseInOut hr = let noncomments = filter ((DTCH_Comment /= ) . label) $ cols hr
                 in if length noncomments > 1 && all ((DTCH_In ==) . label) noncomments
                    then let rightmost = last noncomments
                         in hr { cols = map (\ch -> if ch == rightmost then ch { label = DTCH_Out } else ch) (cols hr) }
                    else hr

mkDataCol :: Maybe DMNType -> String -> ColBody
mkDataCol dmntype = DTCBFeels . mkFs dmntype 
mkDataColComment :: String -> ColBody
mkDataColComment mcs = DTComment (if mcs == "" then Nothing else Just mcs)
