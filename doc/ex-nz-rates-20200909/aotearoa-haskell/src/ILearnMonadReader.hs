
module ILearnMonadReader where

import Data.Map (lookup, Map, fromList)
import Control.Monad.Reader
import Data.Maybe
import Debug.Trace

type LangID = String   -- "en-us", "en-gb"
type NLSyntax = String -- in future this will be some kind of GF structure handling parts of speech, pluralization, gender, etc.

data MyTable = MyTable { tval  :: Int
                       , tdict :: Maybe (Data.Map.Map LangID NLSyntax) }
             deriving (Show)

mkMap = Data.Map.fromList

type Env = Data.Map.Map String MyTable
myenv :: Env
myenv = mkMap [("potato", MyTable 6 $ pure $ mkMap [("latin", "Solanum tuberosum")
                                                  ,("en-us", "spud")
                                                  ,("en-gb", "common potato")
                                                  ,("fr",    "pomme de terre")])
              ,("fish",   MyTable 4 $ pure $ mkMap [("latin","Serranidae")
                                                  ,("en-us","fishie")
                                                  ,("en-gb","ocean-dweller")
                                                  ,("fr",   "poisson")])
              ,("fishie",   MyTable 4 $ pure $ mkMap [("latin","Serranidae")
                                                    ,("en-us","fishono")
                                                    ,("en-gb","ocean-dweller")
                                                    ,("fr",   "poisson")])
              ,("fishono",   MyTable 4 $ pure $ mkMap [("latin","Serranidae")
                                                     ,("en-us","nomore")
                                                     ,("en-gb","ocean-dweller")
                                                     ,("fr",   "poisson")])
              ]

myrecurse :: String -> Reader (Env,LangID) (Maybe [String])
myrecurse x = do
  (env,wanted_lang) <- ask
  return $ do
    mytable <- Data.Map.lookup x env
    let myval = tval mytable
    mydict <- tdict mytable
    myword <- Data.Map.lookup wanted_lang mydict
    return $ [x ++ " has the value " ++ (show myval) ++ " and in " ++ wanted_lang ++ " is called " ++ myword]
      ++ (Data.Maybe.fromMaybe [] $ (runReader $ myrecurse myword) (env,wanted_lang))

mymain x y = do
  (runReader $ myrecurse x) (myenv,y)
