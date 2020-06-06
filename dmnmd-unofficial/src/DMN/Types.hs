{-# LANGUAGE OverloadedStrings #-}

module DMN.Types where

-- definitions common to DecisionTable and DMNParseTable

import Prelude hiding (takeWhile)
import qualified Data.Map as Map
import Data.List.Utils (replace)

data HitPolicy = HP_Unique
               | HP_Any
               | HP_Priority
               | HP_First
               | HP_OutputOrder
               | HP_RuleOrder
               | HP_Collect CollectOperator
               | HP_Aggregate
               deriving (Show, Eq)

data CollectOperator = Collect_Sum -- +
                     | Collect_Min -- <
                     | Collect_Max -- >
                     | Collect_Cnt -- #
                     | Collect_All --
               deriving (Show, Eq)


-- DMN's types are slightly different from ours
data DMNType = DMN_String
             | DMN_Number
             | DMN_Boolean
             | DMN_List DMNType
             deriving (Show, Eq)
baseType :: Maybe DMNType -> Maybe DMNType
baseType Nothing = Just DMN_String
baseType (Just (DMN_List x)) = baseType (Just x)
baseType (Just x) = Just x

type DTvar = String

data FBinOp = Flt | Flte
            | Fgt | Fgte
            | Feq
             deriving (Show, Eq)

data FEELexp = FSection FBinOp DMNVal
             | FInRange Float Float
             | FAnything
             | FNullary DMNVal
             | FFunction FNumFunction
             deriving (Show, Eq)
type SymbolTable = Map.Map String FEELexp

-- once we go higher-order we can do fun things like define ordered semilattices or whatever. for example:
-- http://matt.might.net/articles/partial-orders/
-- fEval (>50)   (100)    = True
-- fEval (>50) (< (>100)) = True

data DTCH_Label = DTCH_Comment
                | DTCH_In
                | DTCH_Out
                 deriving (Show, Eq)

data ColHeader = DTCH { label   :: DTCH_Label
                      , varname :: String
                      , vartype :: Maybe DMNType
                      , enums   :: Maybe [FEELexp] -- ordered list of domain elements seen in the column below; used by HP_OutputOrder
                      }
                 deriving (Show, Eq)

var_name :: ColHeader -> String
var_name = underscore . varname

underscore :: String -> String
underscore = (replace " " "_")

data DecisionTable = DTable { tableName :: String
                             , hitpolicy :: HitPolicy
                             , header    :: [ColHeader]
                             , allrows   :: [DTrow]
                             }
               deriving (Show, Eq)

datarows :: DecisionTable -> [DTrow]
datarows = filter (/= DThr) . allrows

data DTrow = DThr -- horizontal rule, a row of dashes or whatever, used as an inter-row separator sometimese
           | DTrow { row_number   :: Maybe Int
                   , row_inputs   :: [[FEELexp]] -- two-layer input and output to handle : | foo, bar | baz |
                   , row_outputs  :: [[FEELexp]] --                                        [[   ,    ]      ]
                   , row_comments :: [Maybe String] }
           deriving (Show, Eq)


data HeaderRow = DTHR { hrhp :: HitPolicy
                      , cols :: [ColHeader]
                      }
                 deriving (Show, Eq)

type CommentString = String

data ColBody = DTCBFeels [FEELexp] -- inputs and outputs are both FEELexps. lists, in fact, in hxt arrowlist tradition. so multivalues can propagate.
             | DTComment (Maybe CommentString)
                 deriving (Show, Eq)

data FNumFunction = FNF0 DMNVal
                  | FNF1 String
                  | FNF3 FNumFunction FNOp2 FNumFunction
             deriving (Show, Eq)

data FNOp2 = FNMul
           | FNDiv
           | FNPlus
           | FNMinus
           | FNExp
             deriving (Show, Eq)

-- will need some pickle/unpickle infrastructure later
data DMNVal = VS String
            | VN Float
            | VB Bool
            deriving (Show, Eq)

