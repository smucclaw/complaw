{-# LANGUAGE Arrows, NoMonomorphismRestriction, QuasiQuotes, MultiWayIf, OverloadedStrings, DuplicateRecordFields #-}

module Main
where

import System.IO
import Control.Monad
import System.Environment
import Data.List
import Data.Char (toLower, isAlphaNum)
import Data.List (takeWhile)
import Data.List.Utils (replace)
import Data.Maybe
import Text.RawString.QQ
import DMN.Types
import DMN.ParseTable
import DMN.DecisionTable
import DMN.Translate.JS
import DMN.ParseFEEL
import Test.Hspec
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Either
import qualified Options.Applicative as OA
import Options.Applicative (long, short, help, helper, fullDesc, progDesc, strOption, switch, value, info, metavar, str, many, argument)
import Data.Semigroup ((<>))
import Control.Applicative

-- let's do getopt properly

data ArgOptions = ArgOptions
  { verbose  :: Bool
  , halp     :: Bool
  , quiet    :: Bool
  , untyped  :: Bool
  , propstyle :: Bool
  , informat  :: String
  , outformat :: String
  , out      :: String
  , pick     :: String
  , input    :: [String]
  }
  deriving (Show, Eq)

argOptions :: OA.Parser ArgOptions
argOptions = ArgOptions
  <$> switch    (long "verbose"    <> short 'v'                                          <> help "more verbosity" )
  <*> switch    (long "help"       <> short 'h'                                          <> help "dmn2js < file.{org,md} > file.ts" )
  <*> switch    (long "quiet"      <> short 'q'                                          <> help "less verbosity" )
  <*> switch    (long "untyped"    <> short 'u'                                          <> help "no Typescript annotations" )
  <*> switch    (long "props"      <> short 'r'                                          <> help "JS functions use props style" )
  <*> strOption (long "informat"   <> short 'f' <> metavar "InputFormat"  <> value "md"  <> help "input format" )
  <*> strOption (long "outformat"  <> short 't' <> metavar "OutputFormat" <> value "ts"  <> help "output format" )
  <*> strOption (long "out"        <> short 'o' <> metavar "FILE"   <> value "-"         <> help "output file" )
  <*> strOption (long "pick"       <> short 'p' <> metavar "TABLE"  <> value ""          <> help "name of desired decision table" )
  <*> many ( argument str (metavar "FILES..."))

main :: IO ()
main = do
  opts <- OA.execParser $ info (argOptions OA.<**> helper) (fullDesc <> progDesc "DMN CLI interpreter and converter" <> OA.header "dmnmd")
  let infiles = if input opts == [] then ["-"] else input opts
      myerr msg = hPutStrLn stderr msg
      mylog msg = when (verbose opts) $ myerr msg
  forM_ (zipWith (,) [1..] infiles) (
    \(inum,infile) -> do
      mylog $ "* opening file: " ++ infile
      inlines <- if infile == "-" then getContents else readFile infile
      let myresult = parseOnly (grepMarkdown <?> "grepMarkdown") (T.pack inlines)
      either
        (\gulp -> myerr $ "** failed to parse: " ++ gulp)
        (\inputchunks ->
           forM_ (inputchunks) (
            \mychunk -> do
              let myChunkName = (maybe ("f"++show inum)
                                 (\cname -> filter (\x->isAlphaNum x || x=='_') $ replace " " "_" $ unwords $ words cname)
                                 (chunkName mychunk))
              mylog $ "** found table: " ++ myChunkName ++ "\n" ++ unlines (chunkLines mychunk)
              either
                (\myPTfail -> myerr $ "** failed to parse table " ++ myChunkName ++ "   :ERROR:\n" ++ show myPTfail)
                (\dtable -> if ((pick opts == "") || (pick opts == tableName dtable))
                            then putStrLn $ toJS (JSOpts (Main.propstyle opts) (not $ untyped opts)) dtable -- tweak this to respect --out
                            else return ()
                )
                (parseOnly (parseTable myChunkName <?> "parseTable") $ T.pack $ unlines $ chunkLines mychunk)
            )
        ) myresult
    )

--  putStrLn $ toJS (fromRight (error "parse error") (parseOnly (parseTable "mydmn1") dmn2))

type InputChunks = [InputChunk]

data InputChunk = InputChunk
  { chunkName  :: Maybe String
  , chunkLines :: [String]
  }
  deriving (Show, Eq)
-- in future, consider grabbing the tables out of Pandoc -- maybe this would be better off as a JSON filter?

grepMarkdown :: Parser InputChunks
grepMarkdown = many (grepTable <?> "grepTable")

grepTable :: Parser InputChunk
grepTable = do
  mHeader <- (maybeHeaderLines <?> "maybeHeaderLines")
  tablelines <- many1 (getTableLine <?> "getTableLine")
  return (InputChunk mHeader tablelines)
  
getTableLine :: Parser String
getTableLine = do
  pipe <- char '|'
  therest <- manyTill anyChar endOfLine
  return $ pipe : therest

irrelevantLine :: Parser (Maybe String)
irrelevantLine = do
  endOfLine <|> (notChar '|' >> skipWhile (/= '\n') >> endOfLine)
  return Nothing

maybeHeaderLines :: Parser (Maybe String)
maybeHeaderLines = do
  gotHeaders <- catMaybes <$> many maybeHeaderLine
  return (if length gotHeaders > 0 then Just (last gotHeaders) else Nothing)

maybeHeaderLine :: Parser (Maybe String)
maybeHeaderLine = do
  foundLine <- orgNameLine <|> headerLine <|> irrelevantLine
  return $ (Data.List.takeWhile (/=':')) <$> foundLine

headerLine :: Parser (Maybe String)
headerLine = do
  skipMany1 (satisfy (\c -> c == '#' || c == '*'))
  skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)

orgNameLine :: Parser (Maybe String)
orgNameLine = do
  "#+NAME:" >> skipHorizontalSpace
  content <- manyTill anyChar endOfLine
  return (Just content)

