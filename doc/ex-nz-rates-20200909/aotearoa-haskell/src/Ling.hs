{-# LANGUAGE OverloadedStrings #-}

-- things to do with computational linguistics
-- later we may import GF here

module Ling (anaphora) where

import Debug.Trace
import qualified Data.Maybe
import Control.Monad.State as S
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Applicative (liftA3)
import Control.Monad (replicateM)

type Phrasebook = Map.Map Text.Text [Int] -- phrases to the line numbers where they were seen

startstate :: Phrasebook
startstate = Map.fromList [("the rates payable for that rating year in respect of the property",[])
                          ,("the many rates",[])
                          ,("bar",[])
                          ,("foo",[])
                          ,("linenumber",[1])
                          ]

rewrite "the rates payable for that rating year in respect of the property" = Just "those rates"
rewrite "the many rates" = Just "those rates"
rewrite _ = Nothing

anaphorum :: (Text.Text, Int) -> State Phrasebook Text.Text
anaphorum (w,linenum) = do
    pb <- get
    let x = Text.strip w
        whitespace = Text.takeWhile ((==) ' ') w
        n1 = Data.Maybe.fromMaybe w $ do
          mphrase <- Map.lookup x pb
          guard (length mphrase > 0 && head mphrase >= linenum - 5)
          return =<< (whitespace <>) <$> rewrite x
    put (Map.update (Just . (:) linenum) x pb)
    return n1

anaphora :: [Text.Text] -> [Text.Text]
anaphora input = evalState (traverse anaphorum (input `zip` [1..])) startstate 

runlines = Text.unlines $ anaphora $ Text.lines $ ("foo\nbar\nthe many rates\n   the many rates\nfoo\nwhatevers\n") :: Text.Text

-- anaphorise :: [Text.Text] -> State LineContext -> [Text.Text]
-- anaphorise xs 


