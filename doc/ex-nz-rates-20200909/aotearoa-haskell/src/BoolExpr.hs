module BoolExpr where

import Data.Text

data BoolExpr = And      BoolExpr BoolExpr
              | Or       BoolExpr BoolExpr
              | XOr      BoolExpr BoolExpr
              | BoolVar  String
              | Any     [BoolExpr]
              | All     [BoolExpr]
              | Yes
              | No
              deriving (Eq, Ord, Show, Read)

