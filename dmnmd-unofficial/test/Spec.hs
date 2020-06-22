{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}

module Main where

import Control.Monad
import Text.RawString.QQ
import DMN.DecisionTable
import DMN.Types
import DMN.ParseTable
import DMN.ParseFEEL
import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Either (fromRight)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  forM_ [spec1, spec2, spec3] $ hspec
  return ()

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()

spec1 :: Spec
spec1 = do
  describe "parseHelloWorld" $ do
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

parseHelloWorld2 :: Parser Text
parseHelloWorld2 = do
  result <- string "Hello World!"
  return result

spec2 :: Spec
spec2 = do
  describe "parseHelloWorld2" $ do
    it "should parse the phrase 'Hello World!' and return 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld2 `shouldParse` ("Hello World!" :: Text)

spec3 :: Spec
spec3 = do
  describe "parseVarname" $ do
    it "should parse a typical variable name"             $ ("varname" :: Text) ~> parseVarname `shouldParse` ("varname" :: Text)
    it "should parse a variable name with spaces"         $ ("var name" :: Text) ~> parseVarname `shouldParse` ("var name" :: Text)
    it "should fail on a non-variable name (digit first)" $ parseVarname `shouldFailOn` ("123varname" :: Text)
    it "should fail on a non-variable name (dash first)"  $ parseVarname `shouldFailOn` ("- Foovar_" :: Text)
    it "should fail on a blank string"                    $ parseVarname `shouldFailOn` ("" :: Text)
  describe "parseColHeader" $ do
    it "should parse just a column header"                $ ("varname" :: Text) ~> parseColHeader `shouldParse` (DTCH DTCH_In"varname" Nothing Nothing)
  describe "pipeSeparator" $ do
    it "should parse just a single pipe"                  $ (getpipeSeparator >> endOfInput) `shouldSucceedOn` ("|" :: Text)
    it "should not parse more than one pipe"              $ (getpipeSeparator >> endOfInput) `shouldFailOn`    ("||" :: Text)
  describe "getpipeSeparator" $ do
    it "should parse just a single pipe"                   $ ("|" :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with r whitespace" $ ("|   " :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with l whitespace" $ ("   |" :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should parse just a single pipe with 2 whitespace" $ ("  | " :: Text) ~> getpipeSeparator `shouldParse` ("|" :: Text)
    it "should not parse multiple pipes to end"            $ (getpipeSeparator *> endOfInput) `shouldFailOn` ("||" :: Text)
    it "should parse multiple pipes presumably leaving some unconsumed" $ (getpipeSeparator) `shouldSucceedOn` ("||" :: Text)
    it "should leave the text unconsumed"                  $ ("|abc" :: Text) ~?> getpipeSeparator `leavesUnconsumed` ("abc" :: Text)
  describe "parseHeaderRow" $ do
    it "should parse a pipe"                                 $ pipeSeparator `shouldSucceedOn` ("|"::Text)
    it "should parse many pipes"                             $ (many pipeSeparator <* endOfInput) `shouldSucceedOn` ("| |||   |  |  "::Text)
    it "should parse many pipes but not something with text" $ (many pipeSeparator <* endOfInput) `shouldFailOn`    ("| | text |"::Text)
    it "should parse a no-column header row"  $ ("| U |"                       :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [])
    it "should parse a one-column header row" $ ("| U | varname1 |"            :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing])
    it "should parse a two-column header row" $ ("| U | varname1 | varname2 |" :: Text) ~> parseHeaderRow `shouldParse` (DTHR HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing
                                                                                                                                        ,DTCH DTCH_In "varname2" Nothing Nothing])

  describe "parseSubHeadRow" $ do
    it "should parse a continuation row" $
      ("|  | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |\n" :: Text) ~>
      parseContinuationRow `shouldParse` (["LOW, MEDIUM, HIGH", "", "DECLINE, REFER, ACCEPT", "LEVEL 2, LEVEL 1, NONE", ""])

  describe "parseSubHeadRows" $ do
    it "should parse multiple continuation rows, unwrapping along the way" $
      (("|  | LOW, MEDIUM,      |                      | DECLINE,      | LEVEL 2, LEVEL |                             |\n" :: Text) <>
       ("|  |              HIGH |                      | REFER, ACCEPT | 1, NONE        |                             |\n" :: Text)) ~>
      parseContinuationRows `shouldParse` (["LOW, MEDIUM, HIGH", "", "DECLINE, REFER, ACCEPT", "LEVEL 2, LEVEL 1, NONE", ""])

  describe "parse header and subhead together" $ do
    it "should parse an entire header up to the ---- line" $
      (("| O | Age | RiskCategory (out) | DebtReview : Boolean (out)|\n" :: Text) <>
       ("|   |     | LOW, MEDIUM,       | true                      |\n" :: Text) <>
       ("|   |     | HIGH               |                           |\n" :: Text))
      ~> (do
        hr <- parseHeaderRow
        sh <- parseContinuationRows
        return (hr, sh)
      ) `shouldParse` ( (DTHR HP_OutputOrder [ DTCH DTCH_In "Age" Nothing Nothing
                                             , DTCH DTCH_Out "RiskCategory" Nothing Nothing
                                             , DTCH DTCH_Out "DebtReview" (Just DMN_Boolean) Nothing])
                      , [ "", "LOW, MEDIUM, HIGH", "true" ] )

  describe "parse header and subhead and a data row together" $ do
    it "should parse an entire header, the ---- line, and a logical data row below split over two physical rows, then a data row" $
      (("| O | Age : Number | RiskCategory (out) | DebtReview : Boolean (out)|\n" :: Text) <>
       ("|   |              | LOW, MEDIUM,       | true                      |\n" :: Text) <>
       ("|   |              | HIGH               |                           |\n" :: Text) <>
       ("|-------------------------------------------------------------------|\n" :: Text) <>
       ("| 1 | <            | HIGH               |                           |\n" :: Text) <>
       ("|   |   18         |                    | false                     |\n" :: Text) <>
       ("| 2 | >= 21        | LOW                | yes                       |\n" :: Text))
      ~> (do
        hr <- parseHeaderRow
        sh <- parseContinuationRows
        let columnSignatures = columnSigs hr
        hrule <- parseDThr
        dr <- parseDataRows columnSignatures <?> "parseDataRows"
        return (hr, sh, hrule, dr)
      ) `shouldParse` ( (DTHR HP_OutputOrder [ DTCH DTCH_In "Age"           (Just DMN_Number) Nothing
                                             , DTCH DTCH_Out "RiskCategory"  Nothing Nothing
                                             , DTCH DTCH_Out "DebtReview"   (Just DMN_Boolean) Nothing])
                      , [ "", "LOW, MEDIUM, HIGH", "true" ]
                      , DThr
                      , [ DTrow (Just 1) [ mkFs (Just DMN_Number) "<18"  ] [ [ FNullary $ VS "HIGH" ] , [ FNullary $ VB False ] ] []
                        , DTrow (Just 2) [ mkFs (Just DMN_Number) ">=21" ] [ [ FNullary $ VS "LOW" ]  , [ FNullary $ VB True  ] ] []
                        ])

  describe "parseHitPolicy" $ do
    it "should parse a Unique hit policy"        $ ("U"  :: Text) ~> parseHitPolicy `shouldParse` HP_Unique
    it "should parse an Any hit policy"          $ ("A"  :: Text) ~> parseHitPolicy `shouldParse` HP_Any
    it "should parse a Collect All hit policy"   $ ("C"  :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_All
    it "should parse a Collect All hit policy"   $ ("CA" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_All
    it "should parse a Collect Sum hit policy"   $ ("C+" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_Sum
    it "should parse a Collect Count hit policy" $ ("C#" :: Text) ~> parseHitPolicy `shouldParse` HP_Collect Collect_Cnt

  describe "parseTypeDecl" $ do
    it "should parse a String type annotation"  $ (": String"  :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_String)
    it "should parse a Number type annotation"  $ (": Number"  :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_Number)
    it "should parse a Boolean type annotation" $ (": Boolean" :: Text) ~> parseTypeDecl `shouldParse` (Just DMN_Boolean)
    it "should parse a List of Strings"         $ (": [String]" :: Text) ~> parseTypeDecl `shouldParse` (Just (DMN_List DMN_String))

  describe "wtf" $ do
    it "testing many1 digit"
      $ ("| 123 |" :: Text) ~> (
      do
        pipeSeparator
        mymap <- mapM (\x -> do
                          myjust <- Just . (\d -> x * (read d) :: Int) <$> many1 digit
                          pipeSeparator
                          return myjust
                      ) [2]
        return mymap
      ) `shouldParse` ([Just 246])

    it "testing manual recreation of parseDataRow"
      $ ("| 1 |\n" :: Text) ~> (
      do
        pipeSeparator
        myrownumber <-  many1 digit
        pipeSeparator
        endOfLine
        return (DTrow (Just . (\n -> (read n) :: Int) $ myrownumber) [] [] [])
      ) `shouldParse` (DTrow (Just 1) [] [] [])

  describe "parseDataRow" $ do
    it "should parse a zero-column row"
      $ ("| 1 |\n" :: Text ) ~> (parseDataRow []) `shouldParse` (DTrow (Just 1) [] [] [])
    it "should parse a comment-only row"
      $ ("| 1 | rem |\n" :: Text ) ~> (parseDataRow [(DTCH_Comment, Nothing)]) `shouldParse` (DTrow (Just 1) [] [] [Just "rem"])
    it "should parse an input-only row"
      $ ("| 1 | potato |\n" :: Text ) ~> (parseDataRow [(DTCH_In, Nothing)]) `shouldParse` (DTrow (Just 1) [[FNullary $ VS "potato"]] [] [])
    it "should parse an output-only row"
      $ ("| 1 | potato |\n" :: Text ) ~> (parseDataRow [(DTCH_Out, Nothing)]) `shouldParse` (DTrow (Just 1) [] [[FNullary $ VS "potato"]] [])

  describe "parseTable" $ do
    it "should parse a null table with no header columns and no body rows"
      $ ("| U |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [] [])
    it "should parse a boring table with one header column and no body rows"
      $ ("| U | varname1 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing] [])
    it "should parse a boring table with one in header, one explicit out, and no body rows"
      $ ("| U | varname1 | varname2 (out) |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with two in headers, which should autoswitch to out"
      $ ("| U | varname1 | varname2 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with one explicit in headers, and the other which should autoswitch to out"
      $ ("| U | varname1 (in) | varname2 |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Out "varname2" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and no body rows"
      $ ("| U | varname1 | # rem |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Comment "rem" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and no body rows"
      $ ("| U | varname1 | rem (comment) |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse` (DTable "mytable1" HP_Unique [DTCH DTCH_In "varname1" Nothing Nothing, DTCH DTCH_Comment "rem" Nothing Nothing] [])
    it "should parse a boring table with one header column, one comment column, and one body row"
      $ ("| U | varname1 | rem (comment) |\n| 1 | foo | mycomment |\n" :: Text )
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "varname1" (Just DMN_String) Nothing, DTCH DTCH_Comment "rem" Nothing Nothing]
        [DTrow (Just 1) [mkFs (Just DMN_String) "foo"] [] [Just "mycomment"]])
    it "should parse the standard dmn example 1"
      $ dmn1
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DThr
        ,DTrow (Just 1) [mkFs Nothing "Fall"]   [mkFs Nothing "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs Nothing "Winter"] [mkFs Nothing "Roastbeef"] [Nothing]
        ,DTrow (Just 3) [mkFs Nothing "Spring"] [mkFs Nothing "Steak"    ] [Nothing]
        ,DTrow (Just 4) [mkFs Nothing "Summer"] [mkFs Nothing "Light Salad and a nice Steak"] [Just "Hey, why not?"]
        ])

    it "should parse the standard dmn example 1b with comma expressions"
      $ dmn1b
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DThr
        ,DTrow (Just 1) [mkFs Nothing "Fall"]   [mkFs Nothing "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs Nothing "Winter"] [mkFs Nothing "Roastbeef"] [Nothing]
        ,DTrow (Just 3) [[FNullary $ VS "Spring", FNullary $ VS "Summer"]] [mkFs (Just DMN_String) "Stew"    ] [Just "Multivalue"]
        ])

    it "should parse the standard dmn example 1c with type annotations and multivalues"
      $ dmn1c
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_String) "Fall"]   [mkFs (Just DMN_String) "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs (Just DMN_String) "Winter"] [[FNullary $ VS "Roastbeef", FNullary $ VS "Strawberries"]] [Nothing]
        ,DTrow (Just 3) [[FNullary $ VS "Spring", FNullary $ VS "Summer"]] [[FNullary $ VS "Stew"    ]] [Just "Multivalue"]
        ])

    it "should parse the standard dmn example 2 with multiple columns and numeric comparisons"
      $ dmn2
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_Unique
        [DTCH DTCH_In "Season" (Just DMN_String) Nothing, DTCH DTCH_In "guestCount" (Just DMN_Number) Nothing, DTCH DTCH_Out "Dish" (Just DMN_String) Nothing, DTCH DTCH_Comment "Annotation" Nothing Nothing]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_String) "Fall",   mkFs (Just DMN_Number) "<= 8"]    [mkFs (Just DMN_String) "Spareribs"] [Nothing]
        ,DTrow (Just 2) [mkFs (Just DMN_String) "Winter", mkFs (Just DMN_Number) "<= 8"]   [[FNullary $ VS "Roastbeef"]] [Nothing]
        ,DTrow (Just 3) [mkFs (Just DMN_String) "Spring", mkFs (Just DMN_Number) "<= 4"]   [[FNullary $ VS "Dry Aged Gourmet Steak"]] [Nothing]
        ,DTrow (Just 4) [mkFs (Just DMN_String) "Spring", mkFs (Just DMN_Number) "[5..8]"] [[FNullary $ VS "Steak"]] [Nothing]
        ,DTrow (Just 5) [mkFs (Just DMN_String) "Fall, Winter, Spring", mkFs (Just DMN_Number) "> 8"] [[FNullary $ VS "Stew"]] [Nothing]
        ,DTrow (Just 6) [[FNullary $ VS "Summer"], [FAnything]] [mkFs (Just DMN_String) "Light Salad and a nice Steak"] [Just "Hey, why not?"]
        ])

    it "should parse the Collect table with a subheader row"
      $ dmn3a
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_All)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])

    it "should parse the Output Order table with a subheader row"
      $ dmn3b
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" HP_OutputOrder
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])

  describe "evalTable dmn1" $ do
    it "should run standard dmn example 1: Fall -> Spareribs"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should run standard dmn example 1: Winter -> Roastbeef"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]

  describe "evalTable dmn1c" $ do
    it "should run standard dmn example 1c: Fall -> Spareribs"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1c)) (mkFs (Just DMN_String) "Fall")) `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should run standard dmn example 1c: Winter -> [Roastbeef, Strawberries]"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1c)) (mkFs (Just DMN_String) "Winter")) `shouldBe` Right [[[FNullary $ VS "Roastbeef", FNullary $ VS "Strawberries"]]]
    it "should run standard dmn example 1c: Spring -> Stew"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Spring"])) `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should run standard dmn example 1c: Summer -> Stew"
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Summer"])) `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should run standard dmn example 1c: Never -> Left \"no match\""
      $ (evalTable (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn1c)) ([FNullary $ VS "Never"])) `shouldBe` Left "no rows returned -- a unique table should have one result!"


  describe "evalTable dmn2" $ do
    let evaled2 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn2))
    it "should handle multiple inputs: Fall, 5"   $ evalTable evaled2 [FNullary (VS "Fall"),   FNullary (VN 5)] `shouldBe` Right [[[FNullary $ VS "Spareribs"]]]
    it "should handle multiple inputs: Fall, 9"   $ evalTable evaled2 [FNullary (VS "Fall"),   FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Winter, 5" $ evalTable evaled2 [FNullary (VS "Winter"), FNullary (VN 5)] `shouldBe` Right [[[FNullary $ VS "Roastbeef"]]]
    it "should handle multiple inputs: Winter, 9" $ evalTable evaled2 [FNullary (VS "Winter"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Spring, 3" $ evalTable evaled2 [FNullary (VS "Spring"), FNullary (VN 3)] `shouldBe` Right [[[FNullary $ VS "Dry Aged Gourmet Steak"]]]
    it "should handle multiple inputs: Spring, 9" $ evalTable evaled2 [FNullary (VS "Spring"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Stew"]]]
    it "should handle multiple inputs: Summer, 3" $ evalTable evaled2 [FNullary (VS "Summer"), FNullary (VN 3)] `shouldBe` Right [[[FNullary $ VS "Light Salad and a nice Steak"]]]
    it "should handle multiple inputs: Summer, 9" $ evalTable evaled2 [FNullary (VS "Summer"), FNullary (VN 9)] `shouldBe` Right [[[FNullary $ VS "Light Salad and a nice Steak"]]]

  describe "evalTable dmn3a - collect" $ do
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn3a))
    it "should return all rows: 17, HIGH, True"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe`
      Right [ [[FNullary $ VS "ACCEPT"],  [FNullary $ VS "NONE"],    [FNullary $ VS "Acceptable"]]
            , [[FNullary $ VS "DECLINE"], [FNullary $ VS "NONE"],    [FNullary $ VS "Applicant too young"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 1"], [FNullary $ VS "High risk application"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 2"], [FNullary $ VS "Applicant under debt review"]]
            ]

  describe "evalTable dmn3count - collect count" $ do
    it "should parse table 3c as a Collect Count"   $ dmn3count ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Cnt)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_In "RiskCategory"  (Just DMN_String) (Just $ FNullary . VS <$> words "LOW MEDIUM HIGH")
        , DTCH DTCH_In "DebtReview"    (Just DMN_Boolean) Nothing
        , DTCH DTCH_Out "Routing"       (Just DMN_String) (Just $ FNullary . VS <$> words "DECLINE REFER ACCEPT")
        , DTCH DTCH_Out "Review_level"  (Just DMN_String) (Just $ FNullary . VS <$> ["LEVEL 2", "LEVEL 1", "NONE"])
        , DTCH DTCH_Out "Reason"        (Just DMN_String) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [[FAnything], [FAnything], [FAnything]]                   [mkFs (Just DMN_String) "ACCEPT",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Acceptable"]              []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "<18", [FAnything], [FAnything]]  [mkFs (Just DMN_String) "DECLINE",  mkFs (Just DMN_String) "NONE", mkFs (Just DMN_String) "Applicant too young"]    []
        ,DTrow (Just 3) [[FAnything], mkFs (Just DMN_String) "HIGH", [FAnything]] [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 1", mkFs (Just DMN_String) "High risk application"] []
        ,DTrow (Just 4) [[FAnything], [FAnything], [FNullary (VB True)]]          [mkFs (Just DMN_String) "REFER",  mkFs (Just DMN_String) "LEVEL 2", mkFs (Just DMN_String) "Applicant under debt review"]   []
        ])
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn3count))
    it "17 should return a count of 4"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe` Right [ [[FNullary $ VN 4.0 ]] ]
    it "18 should return a count of 3"   $ evalTable evaled3 [FNullary (VN 18.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe` Right [ [[FNullary $ VN 3.0 ]] ]




-- So, for example, if called with Age = 17, Risk category = “HIGH” and Debt review = true, the Routing rules table in Figure 8.19 would return the outputs of all four rules, in the order 2, 4, 3, 1.
-- p 96
  describe "evalTable dmn3 - ordered output" $ do
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn3b))
    it "should return all rows in order of specification in the subheader"   $ evalTable evaled3 [FNullary (VN 17.0),   FNullary (VS "HIGH"), FNullary (VB True)] `shouldBe`
      Right [ [[FNullary $ VS "DECLINE"], [FNullary $ VS "NONE"],    [FNullary $ VS "Applicant too young"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 2"], [FNullary $ VS "Applicant under debt review"]]
            , [[FNullary $ VS "REFER"],   [FNullary $ VS "LEVEL 1"], [FNullary $ VS "High risk application"]]
            , [[FNullary $ VS "ACCEPT"],  [FNullary $ VS "NONE"],    [FNullary $ VS "Acceptable"]]
            ]

  describe "evalTable dmn4sum - collect sum" $ do
    it "should parse the Zelda Collect Sum table"
      $ dmn4sum
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Sum)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn4sum))
    it "17 should return 3 magical objects"    $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  3.0 ]] ]
    it "18 should return 18 magical objects"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN 18.0 ]] ]

  describe "evalTable dmn4d - collect count" $ do
    it "should parse the Zelda Collect Count table"
      $ dmn4count
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Cnt)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn4count))
    it "17 should match 1 row"    $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  1.0 ]] ]
    it "18 should match 2 rows"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN  2.0 ]] ]


  describe "evalTable dmn4d - collect min" $ do
    it "should parse the Zelda Collect Min table"
      $ dmn4min
      ~> (parseTable "mytable1") `shouldParse`
      (DTable "mytable1" (HP_Collect Collect_Min)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Number) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  mkFs (Just DMN_Number) "2"]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  mkFs (Just DMN_Number) "4"]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  mkFs (Just DMN_Number) "6"]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  mkFs (Just DMN_Number) "8"]    []
        ])
    let evaled3 = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn4min))
    it "17 should have min result of 1"   $ evalTable evaled3 [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary $ VN  1.0 ]] ]
    it "18 should have min result of 3"   $ evalTable evaled3 [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary $ VN  3.0 ]] ]


  describe "parseFNumFunction" $ do
    it "should handle a simple string literal"
      $ ("myvar" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF1 "myvar")
    it "should handle a quoted string"
      $ ("\"myvar\"" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VS "myvar")
    it "should handle a literal number"
      $ ("100.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VN 100.0)
    it "should handle a literal bool"
      $ ("False" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF0 $ VB False)
    it "should handle two numbers multiplied"
      $ ("50.0 * 2.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF3 (FNF0 $ VN 50) (FNMul) (FNF0 $ VN 2))
    it "should handle a variable times a number"
      $ ("age * 2.0" :: Text) ~> (parseFNumFunction) `shouldParse` (FNF3 (FNF1 "age") (FNMul) (FNF0 $ VN 2))

  describe "type inference" $ do
    it "should infer [1..2] as a Number"    $ inferType (mkF (Just DMN_String) "[1..2]") `shouldBe` Just DMN_Number
    it "should infer 123 as a Number"       $ inferType (mkF (Just DMN_String) "123")    `shouldBe` Just DMN_Number
    it "should infer >23 as a Number"       $ inferType (mkF (Just DMN_String) ">23")    `shouldBe` Just DMN_Number
    it "should infer <23 as a Number"       $ inferType (mkF (Just DMN_String) "<23")    `shouldBe` Just DMN_Number
    it "should infer \"yes\" as a Boolean"  $ inferType (mkF (Just DMN_String) "yes")    `shouldBe` Just DMN_Boolean
    it "should infer \"no\" as a Boolean"   $ inferType (mkF (Just DMN_String) "yes")    `shouldBe` Just DMN_Boolean
    it "double-quoted string is a String"   $ inferType (mkF (Just DMN_String) "\"quoted\"")    `shouldBe` Just DMN_String
    it "should infer a stringified age * 2 as a Number" $ inferType (mkF (Just DMN_String) "age * 2") `shouldBe` Just DMN_Number
    it "should infer an explicit Function age * 2 as a Number" $ inferType (FFunction (FNF3 (FNF1 "age") FNMul (FNF0 $ VN 2))) `shouldBe` Just DMN_Number
    it "should infer dmn5a as number, number, bool" $
      dmn5a ~> (parseTable "dmn5a") `shouldParse`
      (DTable "dmn5a" (HP_Collect Collect_Max)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_Number) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Boolean) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_Number) "1",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_Number) "3",  [(FNullary $ VB False)]]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_Number) "5",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_Number) "7",  [(FNullary $ VB False)]]    []
        ])
    it "should infer dmn5b as number, number, bool" $
      dmn5b ~> (parseTable "dmn5b") `shouldParse`
      (DTable "dmn5b" (HP_Collect Collect_Max)
        [ DTCH DTCH_In "Age"            (Just DMN_Number) Nothing
        , DTCH DTCH_Out "SpiritOrbs"     (Just DMN_String) Nothing
        , DTCH DTCH_Out "KorokSeeds"     (Just DMN_Boolean) Nothing
        ]
        [DThr
        ,DTrow (Just 1) [mkFs (Just DMN_Number) "<18"]       [mkFs (Just DMN_String) "one",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 2) [mkFs (Just DMN_Number) "[18..21]"]  [mkFs (Just DMN_String) "three",  [(FNullary $ VB False)]]    []
        ,DTrow (Just 3) [mkFs (Just DMN_Number) ">=18"]      [mkFs (Just DMN_String) "five",  [(FNullary $ VB True )]]    []
        ,DTrow (Just 4) [mkFs (Just DMN_Number) ">=65"]      [mkFs (Just DMN_String) "seven",  [(FNullary $ VB False)]]    []
        ])

  describe "function evaluation " $ do
    let evaled = (fromRight (error "parse error") (parseOnly (parseTable "mytable1") dmn6a))
    it "17 should have result of 0"      $ evalTable evaled [FNullary (VN 17.0)] `shouldBe` Right [ [[FNullary (VB False)], [FNullary $ VN    0.0 ]] ]
    it "18 should have result of 750"    $ evalTable evaled [FNullary (VN 18.0)] `shouldBe` Right [ [[FNullary (VB True)],  [FNullary $ VN  750.0 ]] ]
    it "30 should have result of 3000"   $ evalTable evaled [FNullary (VN 30.0)] `shouldBe` Right [ [[FNullary (VB True)],  [FNullary $ VN 3000.0 ]] ]



dmn1 = T.pack $ dropWhile (=='\n') [r|
| U | Season | Dish                         | # Annotation  |
|---+--------+------------------------------+---------------|
| 1 | Fall   | Spareribs                    |               |
| 2 | Winter | Roastbeef                    |               |
| 3 | Spring | Steak                        |               |
| 4 | Summer | Light Salad and a nice Steak | Hey, why not? |
|]

-- with multivalues in the input
dmn1b = T.pack $ dropWhile (=='\n') [r|
| U | Season               | Dish                         | # Annotation  |
|---+----------------------+------------------------------+---------------|
| 1 | Fall                 | Spareribs                    |               |
| 2 | Winter               | Roastbeef                    |               |
| 3 | Spring, Summer       | Stew                         | Multivalue    |
|]

-- with type annotations multivalues in the input
dmn1c = T.pack $ dropWhile (=='\n') [r|
| U | Season : String      | Dish : String                | # Annotation  |
|---+----------------------+------------------------------+---------------|
| 1 | Fall                 | Spareribs                    |               |
| 2 | Winter               | Roastbeef, Strawberries      |               |
| 3 | Spring, Summer       | Stew                         | Multivalue    |
|]

dmn2 = T.pack $ dropWhile (=='\n') [r|
| U | Season : String      | guestCount : Number  | Dish : String                | # Annotation  |
|---+----------------------+----------------------+------------------------------+---------------|
| 1 | Fall                 | <= 8                 | Spareribs                    |               |
| 2 | Winter               | <= 8                 | Roastbeef                    |               |
| 3 | Spring               | <= 4                 | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]               | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8                  | Stew                         |               |
| 6 | Summer               | -                    | Light Salad and a nice Steak | Hey, why not? |
|]

dmn3a = T.pack $ dropWhile (=='\n') [r|
| C | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)                |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
|---|--------------|-------------------|----------------------|------------------------|------------------------|-----------------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18          |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |              |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |
|]

dmn3b = T.pack $ dropWhile (=='\n') [r|
| O | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)                |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
|---|--------------|-------------------|----------------------|------------------------|------------------------|-----------------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18          |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |              |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |
|]

dmn3count = T.pack $ dropWhile (=='\n') [r|
| C# | Age : Number | RiskCategory      | DebtReview : Boolean | Routing (out)          | Review_level (out)     | Reason (out)                |
|   |              | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
|---|--------------|-------------------|----------------------|------------------------|------------------------|-----------------------------|
| 1 |              |                   |                      | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18          |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |              | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |              |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |
|]


dmn4sum = T.pack $ dropWhile (=='\n') [r|
| C+ | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4count = T.pack $ dropWhile (=='\n') [r|
| C# | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4min = T.pack $ dropWhile (=='\n') [r|
| C< | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn4max = T.pack $ dropWhile (=='\n') [r|
| C> | Age : Number | SpiritOrbs : Number (out) | KorokSeeds : Number (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   2 |
|  2 | [18..21]     |                   3 |                   4 |
|  3 | >=18         |                   5 |                   6 |
|  4 | >=65         |                   7 |                   8 |
|]

dmn5a = T.pack $ dropWhile (=='\n') [r|
| C> | Age  | SpiritOrbs (out) | KorokSeeds (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   1 |                   true |
|  2 | [18..21]     |                   3 |                   false |
|  3 | >=18         |                   5 |                   yes |
|  4 | >=65         |                   7 |                   no |
|]

dmn5b = T.pack $ dropWhile (=='\n') [r|
| C> | Age  | SpiritOrbs (out) | KorokSeeds (out) |
|----+--------------+---------------------+---------------------|
|  1 | <18          |                   one |                   true |
|  2 | [18..21]     |                   three |                   false |
|  3 | >=18         |                   five |                   yes |
|  4 | >=65         |                   seven |                   no |
|]

dmn6a = T.pack $ dropWhile (=='\n') [r|
| F | age : Number | mayBuy : Boolean (out) | limit : Number (out) |
|---+--------------+------------------------+----------------------|
| 1 | <18          | False                  | 0                    |
| 2 | [18..21]     | True                   | 750                  |
| 3 | [21..25]     | True                   | 1500                 |
| 4 | >25          | True                   | age * 100            |
|]
