{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- DO NOT EDIT THIS FILE!
-- direct edits will be clobbered.
-- 
-- this file is autogenerated by tangling ex-20200802-safe-events/README.org
-- open the README.org in emacs and hit C-c C-v t to regenerate this file.

module Main where

import Test.Hspec
import Data.Maybe
import Data.Map
import Control.Monad
import SAFE.Events
import Data.Tree
import Data.Tree.Pretty
import Control.Arrow
import Debug.Trace

main :: IO ()
main = do
  forM_ [spec1] $ hspec
  return ()

acme = Party "Acme Inc." $ fromList (
  ["address" .= MyString "1 Monopoly Way"
  ,"state"   .= MyString "DE"
  ,"country" .= MyString "US"
  ,"bank"    .= MyString "WellsCitiChartered"
  ,"acct"    .= MyString "123-45-6789"])

rich = Party "Richard Moneybags III" $ fromList (
  ["address" .= MyString "1 Capitalist Way"
  ,"state"   .= MyString "NV"
  ,"country" .= MyString "US"
  ,"bank"    .= MyString "StandardFargoBank"
  ,"acct"    .= MyString "888-444-666"])
x .= y = (x,y)
mytxns = mktxns acme [(rich,100000)] (2020,1,2) 10.0
myFinancing1 = Imbued mytxns $ fromList
  [ ("bona fide",          MyBool True)
  , ("arity",              MyInt (length mytxns))
  , ("principal purpose",  MyString "raising capital")
  ]
-- a thing is an Equity Financing if ...
isEF :: Imbued [Transaction] -> Bool
isEF im = and [ attrs im ! "bona fide"                        == MyBool True
              , myint (attrs im ! "arity")                    >= 1
              , attrs im ! "principal purpose"                == MyString "raising capital"
              , allActionVal "issue shares" "issues"          (== MyBool True)
              , allActionVal "issue shares" "sells"           (== MyBool True)
              , allActionVal "issue shares" "security"        (== MyString "preferred")
              , allActionVal "issue shares" "valuation_fixed" (== MyBool True)
              , allActionVal "issue shares" "val_p_p_o"       (`elem` [MyString x | x <- ["pre_money", "post_money", "other"]])
              ]
  where allActionVal n k p = -- the list comprehension below unwraps a seven-layer burrito. well, maybe five.
          all p $ [ params a ! k | txn                        <- underlying im
                                 , MkCl { actions = actions } <- flatten (getAgreement txn)
                                 , a                          <- actions
                                 , a.name == n -- we filter for the desired action name
                                 ]

-- todo: consider hxt's arrowlist approach to tree traversal and matching with >>>


spec1 :: Spec
spec1 = do
  describe "equity financing" $ do
    it "should consider the transactions to be an equity financing" $
      isEF myFinancing1 `shouldBe` True