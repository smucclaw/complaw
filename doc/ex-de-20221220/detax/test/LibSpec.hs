module LibSpec where

import Test.Hspec
import qualified Data.Map as Map
import Lib
import Explanation
import Control.Monad.RWS
import Data.Tree (Tree(Node))

scenarios = Map.fromList
    [ "1a" ~=>
        [ "ordinary income"      <-~ ["Rents" ~== 72150, "Agriculture" ~== 30000  , "Exempt Capital" ~== 100]
        , "extraordinary income" <-~ [                   "Agriculture" ~== 270000 , "Exempt Capital" ~== 100]
        , "ordinary expenses"    <-~ ["Rents" ~== 2150 , "Independent" ~== 6000   , "Other"          ~== 100000 ]
        ]
    , "1b" ~=>
        [ "ordinary income"      <-~ ["Rents" ~== 72150, "Agriculture" ~== 20000  , "Exempt Capital" ~== 100 ]
        , "ordinary expenses"    <-~ ["Rents" ~== 2150,  "Independent" ~== 6000   , "Other"          ~== 60000 ]
        ]
    , "2" ~=>
        [ "ordinary income"      <-~ [ "Rents" ~== 72150, "Agriculture" ~== 30000 , "Exempt Capital" ~== 100 ]
        , "extraordinary income" <-~ [                    "Agriculture" ~== 270000, "Exempt Capital" ~== 100 ]
        , "ordinary expenses"    <-~ [ "Rents" ~== 2150 , "Independent" ~== 6000 ] ]

    , "test case 1" ~=> [ "ordinary income"      <-~ [ "Rents"   ~== 72150 ]
                        , "extraordinary income" <-~ [ "Rents"   ~== 25000 ] ]
    , "test case 2" ~=> [ "ordinary income"      <-~ [                       "Trade"   ~== 5350   ]
                        , "ordinary expenses"    <-~ [ "Rents"   ~== 45000  ]
                        , "special expenses"     <-~ [ "Rents"   ~== 3200   ]
                        , "extraordinary income" <-~ [                                               "Capital" ~== 225000 ] ]
    , "test case 3" ~=> [ "ordinary income"      <-~ [                       "Trade"   ~== 5350  ]
                        , "ordinary expenses"    <-~ [ "Rents"   ~== 45000 ]
                        , "special expenses"     <-~ [ "Rents"   ~== 3200  ]
                        , "extraordinary income" <-~ [                                               "Capital" ~== 225000 ] ]

    , "test case 3 - fired"   ~=> [ "ordinary income"      <-~ [ "Employment" ~==  22000 ]
                                    , "extraordinary income" <-~ [ "Employment" ~== 130000 ] ]
    , "test case 3 - unfired" ~=> [ "ordinary income"      <-~ [ "Employment" ~==  22000 ] ]
    ]

spec :: Spec
spec = do
  describe "labelFirst" $ do
    let someScenario = scenarios Map.! "1a"

    it "* the sum of all positive elements, ignoring negative elements" $ do
        -- _ <- xplainF someScenario $ sumOf $ negativeElementsOf [-2, -1, 0, 1, 2, 3]
        ((val, xpl), stab, wlog) <- runRWST
                                        (eval $ sumOf $ negativeElementsOf [-2, -1, 0, 1, 2, 3])
                                        (([],["toplevel"]), someScenario)
                                        (MyState Map.empty Map.empty)
        val `shouldBe` -3.0
        -- xpl `shouldBe` Node ([],[]) []
        stab  `shouldBe` MyState Map.empty Map.empty
        wlog `shouldBe` []