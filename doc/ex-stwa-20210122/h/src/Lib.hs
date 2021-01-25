{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.List (nub, permutations, sort, sortOn, intercalate, foldl')
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified Text.PrettyPrint.Boxes as Bx
type Parser = Parsec Void Text

someFunc :: IO ()
someFunc = do
  myinput <- getContents
  let ast = [ case parse parseBody "parsing section body line" bodyline of
                Left  someError  -> error $ errorBundlePretty someError
                Right rhs -> rhs
            | (title, body) <- sections myinput
            , unpack title `elem` words "Setup Constraints"
            , bodyline      <- body
            , (not . null . unpack) bodyline
            ]
  -- print ast
  solver ast

sc :: Parser ()
sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

data Constraint = MkRelation (String, String, String) -- Person, "one or more", Team
                | MkMember (String, [String])         -- (CLM, [Meng, WeiShen])
                | MkDetail (String, Int, [String])    -- Groups 2 [A, B]
                | MkDefine (String, String)           -- (total population, number of Persons)
                | MkLimit  (String, String, String)   -- Group, 50/100, Person
                deriving (Show, Eq)

type Solution = [[Team]]
type Group  = (GroupName, [Team]);  type GroupName = String
type Team   = (TeamName, [Person]); type TeamName = String
type Person = String
-- yeah, I could've used records, I know
getGroupName :: Group -> GroupName
getGroupName = fst
getGroupTeams :: Group -> [Team]
getGroupTeams = snd
getTeamName :: Team -> TeamName
getTeamName = fst
getMembers  :: Team -> [Person]
getMembers  = snd

orgfile :: Parser [(Text, [Text])]
orgfile = many nonStarLine *> many section <* eof

section = do
  lexeme (some "*")
  title <- nonStarLine
  body  <- many nonStarLine
  return (title, body)

nonStarLine :: Parser Text
nonStarLine = notFollowedBy "*" *> takeWhileP Nothing (/= '\n') <* newline -- anything but "* ..."

sections :: String -> [(Text,[Text])]
sections inputFile =
  case parse orgfile "orgFileParser" (pack inputFile) of
    Left someError -> fail $ errorBundlePretty someError
    Right rhs      -> rhs

parseBody = lexeme li
            >> choice [ try parseRelation
                      , try parseSetup
                      , try parseDefine
                      , try parseCategoryDetail
                      , try parseLimit ]
            <* eof

parseCategoryDetail = do
  lexeme "There" >> lexeme ("is" <|> "are")
  cardinality <- lexeme (some digitChar)
  container <- lexeme (some alphaNumChar) <* lexeme ":"
  labels <- many (lexeme (some alphaNumChar <* many ","))
  return $ MkDetail (container, read cardinality, labels)

parseRelation = do -- Every Person belongs to one or more Teams
  every <- lexeme "Every" *> lexeme (some alphaNumChar)
  lexeme ("belongs to")
  cardinality <- lexeme "one or more" <|> lexeme "exactly one"
  container <- lexeme (some alphaNumChar)
  return $ MkRelation (every, unpack cardinality, singular container)

parseSetup = do
  teamName <- lexeme "Team" *> lexeme (some alphaNumChar)
  lexeme "contains"
  members <- many (lexeme (some alphaNumChar) <* lexeme (many ","))
  return $ MkMember (teamName, members)

parseDefine = do
  subj <- lexeme "The total population"
  lexeme "is"
  obj <- lexeme "the number of Persons"
  return $ MkDefine (toLower <$> unpack subj
                    ,toLower <$> unpack obj)

parseLimit = do
  container <- lexeme "Every" *> lexeme (some alphaNumChar)
  lexeme "contains"
  amount <- lexeme "at most half"
  lexeme "of"
  denominator <- lexeme "the total population"
  return $ MkLimit (container, unpack amount, unpack denominator)

singular word = if last word == 's' then init word else word

li = some alphaNumChar <* "."



maxsize :: [Constraint] -> Int -> Int
maxsize constraints imax = (ceiling $ fromIntegral numPersons / fromIntegral imax)
  where numPersons = length $ persons constraints

persons constraints = nub $ concat [ persons
                                   | (MkMember (teamName, persons)) <- constraints ]

solver :: [Constraint] -> IO ()
solver constraints = do
  -- putStrLn "solving!"
  -- putStrLn $ unwords $ [ "we know about", (show $ length $ persons constraints), "persons:" ]
  --                      ++ persons constraints
  let ms = maxsize constraints imax
      gnames = head [ ab | (MkDetail ("Groups", _, ab)) <- constraints ]
  -- putStrLn $ "each group should contain at most " ++ show ms ++ " persons"
  gss <- solutions ms constraints
  -- putStrLn $ show (length gss) ++ " solutions found."
  forM_ (zip [1..] gss) $ \(gsi, gs) -> do
    Bx.printBox $ Bx.hsep 3 Bx.top [ Bx.text (show gsi ++ ":")
                                   , (Bx.hsep 5 Bx.top (bxGroup <$> (zip gnames gs)))
                                   ]
    putStrLn ""
    where
      imax = head [ i | (MkDetail ("Groups", i, teams)) <- constraints ]
      bxGroup group = foldl (Bx.<+>) (Bx.text $ "Group " ++ getGroupName group ++ ":") (showTeam <$> getGroupTeams group)
      showTeam (teamName, teamMembers) = foldl (Bx.//) Bx.nullBox (Bx.text <$> (teamName : teamMembers))

solutions :: Int -> [Constraint] -> IO [Solution]
solutions maxsize constraints = do
  let cteams = [ (t, members) :: Team
               | (MkMember (t, members)) <- constraints ]
      total = length cteams
      perms = permutations cteams
      splits = [ [groupA, groupB]
               | perm  <- perms
               , pivot <- [1..total-1]
               , let groupA   = sortOn getTeamName $ take pivot perm
                     groupB   = sortOn getTeamName $ drop pivot perm
               ]
      splits2 = nub $ sort <$> splits
      splits3 = [ [groupA, groupB]
                | [groupA, groupB] <- splits2
                , let groupAms = nub $ concatMap getMembers groupA
                      groupBms = nub $ concatMap getMembers groupB
                      hConstraints = [ case c of
                                         MkRelation("Person","exactly one","Group") -> everyIndividualIsInOnlyOneGroup [groupA, groupB]
                                         _                                          -> True
                                     | c <- constraints ]
                , length groupAms <= maxsize
                , length groupBms <= maxsize
                , and $ hConstraints
                ]
      splits4 = nub $ sort <$> splits3

  -- putStrLn $ "we have " ++ show total ++ " cteams = " ++ show cteams
  putStrLn $ "initially " ++ show (length $ perms) ++ " permutations"
  putStrLn $ "considering " ++ show (length $ splits2) ++ " uniques"
  putStrLn $ "considering " ++ show (length $ splits3) ++ " right-sized uniques"
  putStrLn $ "returning " ++ show (length $ splits4) ++ " solutions"
  return splits4
  where
    everyIndividualIsInOnlyOneGroup gs =
      let gPersons   :: [[Person]] = (nub . concatMap getMembers) <$> gs
          allPersons ::  [Person]  =  nub $ concat gPersons
      in all (<= 1) [ length $ [ p
                               | g  <- gPersons -- each group A and B
                               , p `elem` g ]
                    | p <- allPersons ]
