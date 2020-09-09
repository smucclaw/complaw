{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- usage:

-- 20191121-14:27:58 mengwong@venice4:~/src/l/complaw-deeptech/ontologies/rules/aotearoa-haskell% stack exec aotearoa-exe -- combined_income=20000 dependants=0 rates_total=2000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=l4/from-openfisca-rr.l4 --nlgstyle=concrete

-- :main -v combined_income=32103 dependants=0 rates_total=2000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=620 income_threshold=24790 --goal=rr-br3nda

-- :main -v  additional_per_dependant=500 initial_contribution=160 maximum_allowable=620 income_threshold=24790 rates_total=400 combined_income=24000 dependants=2 --goal=rr-br3nda-noclip --nlgstyle=neutral
-- the goal you're looking for is
-- | the lesser of
-- | | the greater of
-- | | simply 0.00
-- | | and
-- | | | the difference between
-- | | | | the excess rates amount, which is
-- | | | | two-thirds
-- | | | | | the difference between
-- | | | | | | the total rates for the property
-- | | | | | and
-- | | | | | | the initial contribution by ratepayer, which is 160.00
-- | | | and
-- | | | | the income taper amount, which is
-- | | | | the quotient given by
-- | | | | | the income component adjusted for dependants, which is
-- | | | | | the difference between
-- | | | | | | the ratepayer's income for the preceding tax year
-- | | | | | and
-- | | | | | | the income taper trigger, which is
-- | | | | | | the sum of
-- | | | | | | | the income threshold, which is 24790.00
-- | | | | | | with
-- | | | | | | | the product of
-- | | | | | | | | the additional allowable income per dependant, which is 500.00
-- | | | | | | | multiplied by
-- | | | | | | | | the number of Persons classified as dependant for the purposes of Rates Rebates
-- | | | | divided by
-- | | | | simply 8.00
-- | and
-- | | the maximum rebate allowed, which is 620.00
-- 
-- :main -v  additional_per_dependant=500 initial_contribution=160 maximum_allowable=620 income_threshold=24790 rates_total=100,200..2000 combined_income=12000,13000..30000 dependants=2 --goal=rr-br3nda-clip 
-- +--------------------------------------------------------------------------------------------------+
-- |       100 200 300 400 500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 |
-- | 12000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 13000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 14000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 15000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 16000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 17000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 18000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 19000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 20000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 21000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 22000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 23000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 24000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 25000   0  27  93 160 227 293 360 427 493  560  620  620  620  620  620  620  620  620  620  620 |
-- | 26000   0   0  67 134 200 267 334 400 467  534  600  620  620  620  620  620  620  620  620  620 |
-- | 27000   0   0   0   9  75 142 209 275 342  409  475  542  609  620  620  620  620  620  620  620 |
-- | 28000   0   0   0   0   0  17  84 150 217  284  350  417  484  550  617  620  620  620  620  620 |
-- | 29000   0   0   0   0   0   0   0  25  92  159  225  292  359  425  492  559  620  620  620  620 |
-- | 30000   0   0   0   0   0   0   0   0   0   34  100  167  234  300  367  434  500  567  620  620 |
-- +--------------------------------------------------------------------------------------------------+
-- ## 380 answers across 20 variations of rates_total * 19 variations of combined_income


import Lib
import Ling
import Debug.Trace
import Data.Char
import Data.Matrix
import Data.List
import qualified Data.Text as Text
import Text.Read
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.Exit
import System.IO
import Text.Printf
import Text.Regex
import Text.PrettyPrint.Boxes as Box

data Flag = Quiet               -- -q
          | Verbose             -- -v
          | Help                -- --help
          | Goal String          -- --goal
          | NLGStyle (Maybe String)          -- --style
          | PySource (Maybe String)        -- --py
          | YAMLParamFile (Maybe String)    -- --params
          | L4GoalFile (Maybe String) -- l4igoal
          deriving (Eq,Ord,Show)

flags =
       [Option ['q'] ["quiet"]    (NoArg Quiet)                    "Quiet"
       ,Option ['v'] ["verbose"]  (NoArg Verbose)                  "Verbose"
       ,Option ['g'] ["goal"]     (ReqArg Goal "Goal")             "Goal"
       ,Option ['n'] ["nlgstyle"] (OptArg NLGStyle "NLGStyle")     "Natural Language Generation Style"
       ,Option ['p'] ["pysource"] (OptArg PySource "PySource")     "Python Source"
       ,Option ['y'] ["params"]   (OptArg YAMLParamFile "Params")  "Params File"
       ,Option ['f'] ["l4goalfile"] (OptArg L4GoalFile "Params")   "L4 Goal File"
       ,Option ['h'] ["help"]     (NoArg Help)                     "Print this help message"
       ]

parseargs argv = case getOpt Permute flags argv of
  (flagargs,fs,[]) -> do
    let dataargs = if Data.List.null fs then ["-"] else fs
    if Help `elem` flagargs
      then do hPutStrLn stderr (usageInfo header flags)
              exitWith ExitSuccess
      else return (nub (concatMap set flagargs), dataargs)

  (_,_,errs)      -> do
    hPutStrLn stderr (concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)

  where header = "Usage: l4fisca [-qv] -g concrete|abstract|neutral|algebra --goal=mygoal1 foo=bar baz=quux"
        set f      = [f]

splitOn   = splitRegex . mkRegex
splitEq f = f <$> (Main.splitOn "=")

mkFSFD x y = (FS (Text.pack x), FD y)

splitToFSFD = splitEq (\a -> mkFSFD (a !! 0) (read (a !! 1) :: Double))

main :: IO ()
main = do
  (flagargs, dataargs) <- getArgs >>= parseargs
--  putStrLn $ "Flags: " ++ show flagargs
--  putStrLn $ "Dataargs: " ++ show dataargs

  -- if an L4GoalFile was provided, add it to the map of available goals
  let goalfile = Data.List.find (\case (L4GoalFile _) -> True; _ -> False) flagargs
      goal = Data.List.find wantgoal flagargs
      goalstr = getgoal goal
  fileExists <- System.Directory.doesFileExist goalstr
  goalexpr <- (if fileExists
                  then do contents <- readFile goalstr
                          return $ ((read contents) :: MathExpr Double)
                  else return (Data.Maybe.fromMaybe
                               (error $ "goal not found; specify a filename or choose one of " ++ show (Map.keys goals))
                               (Map.lookup goalstr goals)))

  let style = Data.List.find (\arg -> case arg of (NLGStyle _) -> True; _ -> False) flagargs
      styleval = do
        (NLGStyle x) <- style
        y <- x
        (Map.lookup (Text.pack y) nlgstyles)

  -- if any input variables in the scenario have a range operator ".." in their value, then
  -- respect [x,y..z] syntax from Haskell.
  -- for example, dependants=0..4 combined_income=0,500..30000 rates_total=0,200..2000
  --    if there is  one such range,  draw a one-dimensional table down the page
  --    if there are two such ranges, draw a two-dimensional table (using the boxes library)
  --    if there are three or more such ranges, just blast out the x1=y1 x2=y2 x3=y3 answer=N
  -- note: you want to leave --nlgstyle unspecified, or you'll get some really verbose output.

  let (rangeArgs,plainArgs) = Data.List.partition (\(k,v) -> any (`isInfixOf` v) [",",".."]) (splitEq (\a -> (a !! 0, a !! 1)) <$> dataargs)
  
  -- show single scenario
  when (rangeArgs == []) $ do
    let scenario = Map.fromList $ (\(a,b) -> mkFSFD a ((read b) :: Double)) <$> plainArgs
        obfusgoal = case styleval of
--          Just VObfuscated ->   obfuscate goalexpr
          Just VObfuscated ->             goalexpr
          _                -> deobfuscate goalexpr
        answer = runScenario (Env styleval scenario variables_dict) obfusgoal
    when (styleval /= Just VYAML) $ putStrLn $ Text.unpack $ answer
    when (styleval == Just VYAML) $ putStrLn $ dumpYAML scenario answer

  when (rangeArgs /= []) $ do
    -- putStrLn $ "## input contains range! the range args are " ++ show rangeArgs
    -- well, if the list comprehension fits, wear it
    let rangeScenarios = rangeArgs
    let ranges = do
          rangearg <- rangeScenarios
          let rangekey = fst rangearg
              rangeval = snd rangearg
              myrange = case parseRange rangeval of
                Just [(Just x)] -> [x]
                Just [(Just x),(Just y),Nothing] -> [x,y]
                Just [(Just x),Nothing,(Just y)] -> [x..y]
                Just [(Just x),(Just y),(Just z)] -> [x,y..z]
                _ -> error $ "can't parse range out of " ++ rangeval
          return $ (rangekey, myrange)
    let answers = do
          let rangekeys = fst <$> ranges
              rangevals = snd <$> ranges
              -- now we zip back the rangekeys so we know which is which
          -- (\a -> zip (fst a) (snd a) ) <$> (zip (repeat ["A","B","C"]) (sequence [[1,2,3], [20,30],[100]]))
          -- [[("A",1),("B",20),("C",100)],[("A",1),("B",30),("C",100)],[("A",2),("B",20),("C",100)],[("A",2),("B",30),("C",100)],[("A",3),("B",20),("C",100)],[("A",3),("B",30),("C",100)]]

              zipped = (\a -> Data.List.zip (fst a) (snd a)) <$> (Data.List.zip (repeat rangekeys) (sequence rangevals))
          eachthing <- zipped
          let scenario = ( Map.union
                   (Map.fromList $ (\(a,b) -> mkFSFD a (fromIntegral b))   <$> eachthing)
                   (Map.fromList $ (\(a,b) -> mkFSFD a ((read b)::Double)) <$> plainArgs) )
          let myenv = (Env Nothing scenario variables_dict)
          let answer = runScenario myenv goalexpr
          return $ Answer scenario answer $ eachthing ++ pure ("goal", (round ((read $ Text.unpack answer)::Double))::Int)
          -- the scenario overlaps with details. this is intentional: the details are the things that range, that we want to show in the table.

    let mymatrix = Data.Matrix.fromLists $ ((fmap snd) . details) <$> answers

    when (styleval == Just VYAML) $ putStrLn $ concat $ do
      answer <- answers
      let scenario = scn answer
      return $ dumpYAML scenario (Text.pack $ show $ fromJust (lookup "goal" $ details answer))

-- - name: Someone earning 32103 with no dependants and rates of 2000
--   period: 2018
--   absolute_error_margin: 1
--   input:
--     rates_rebates__combined_income: 32103
--     rates_rebates__dependants: 0
--     rates_rebates__rates_total: 2000
--   output:
--     rates_rebates__rebate: 312.67

    when (styleval /= Just VYAML) $ case length ranges of
      2 -> -- data table format. each details contains exactly three elements: the x, the y, and the answer-goal.
        -- coltitles go across the top, but in practice these get glued on top of each individual column of answers, so we don't fold-join them here just yet
        let rowtitles = (text "") : ((text . show) <$> (nub $ snd <$> (!! 1) <$> (details <$> answers)))
      -- rowtitles go on the left
            coltitles = (text . show) <$> (nub $ snd <$> (!! 0) <$> (details <$> answers))
            datacols = Data.List.foldl (/>/) nullBox <$> chunksOf (length rowtitles - 1) ((text . show) <$> (snd <$> (!! 2) <$> (details <$> answers)))
        in prettyPrintBox coltitles (Data.List.foldl (/>/) nullBox rowtitles) datacols
      _ -> -- ordinary "decision table"
        let colnames  = (text . fst) <$> (details $ head answers)
            datacols  = fmap (\col -> Data.List.foldl (/>/) nullBox (fmap (text . show) col)) (Data.Matrix.toLists $ Data.Matrix.transpose mymatrix)
        in prettyPrintBox colnames nullBox datacols

--    putStrLn $ "## " ++ (show $ length answers) ++ " answers across " ++ ( Data.List.intercalate " * " $ (\(rk, myr) -> (show $ length $ myr) ++ " variations of " ++ rk)  <$> ranges )
        
      where
        wantgoal arg = case arg of
          (Goal goal) -> True
          _ ->           False
        getgoal goalarg = case goalarg of
          (Just (Goal goal)) -> goal
          _ -> (error $ "specify a goal name with --goal=mygoalname; choose one of " ++ show (Map.keys goals))
        parseRange :: String -> Maybe [Maybe Int]
        parseRange x = (\ms -> readMaybe <$> [ms !! 0, ms !! 2, ms !! 4])
          <$> (matchRegex (mkRegex "([0-9]+)(,([0-9]+))?(\\.\\.([0-9]+))?") x)
          
        prettyPrintBox toprow leftcol datacols =
          let 
            headeredcols = Data.List.zipWith (/>/) toprow datacols
            tablerows = Data.List.foldl (Box.<+>) leftcol headeredcols
          in printBox $ surround tablerows
        dumpYAML scenario goalvalue = unlines $ do
          let unfd     = floor . (\(FD y) -> y) . fromJust
          [unwords ["- name: Someone"
                           ,"with " ++ (show $ unfd $ Map.lookup (FS "dependants") scenario) ++ " dependants"
                           ,"earning"
                           ,"$" ++ (show $ unfd $ Map.lookup (FS "combined_income") scenario)
                           ,"who paid $" ++ (show $ unfd $ Map.lookup (FS "rates_total") scenario) ++ " in rates"
                           ]]
            ++ ["  period: " ++ (show $ unfd $ Map.lookup (FS "period") scenario)
               ,"  absolute_error_margin: 1"
               ,"  input:" ]
            ++ [ "    rates_rebates__" ++ varname ++ ": " ++ (show $ unfd $ Map.lookup (FS $ Text.pack varname) scenario)
               | varname <- words "combined_income dependants rates_total" ]
            ++ ["  output:"
               ,"    rates_rebates__rebate: " ++ (Text.unpack goalvalue)
               ]

-- | Paste two boxes together vertically, using a default (left)
--   alignment.
(/>/) :: Box -> Box -> Box
t />/ b = vcat right [t,b]


data Answer = Answer { scn :: RatesRebateWorld
                     , ans :: Text.Text
                     , details :: [(String,Int)]
                     }


-- TOOD: move this out to an output rendering library
-- all this is from the boxes library
-- https://www.reddit.com/r/haskell/comments/4222im/how_to_make_this_tableheaders_work_with_the_boxes/
-- https://github.com/treeowl/boxes/blob/master/Text/PrettyPrint/Boxes.hs

vDiv :: Int -> Box
vDiv n = vcat left (replicate n (char '|'))

hDiv :: Int -> Box
hDiv n = hcat left (replicate n (char '-'))

surround :: Box -> Box
surround box = let r = rows box
                   c = cols box + 2
               in  (char '+' Box.<> hDiv c  Box.<> char '+')
                // (vDiv r   Box.<+> box     Box.<+> vDiv r  )
                // (char '+' Box.<> hDiv c  Box.<> char '+')


-- 17:43 < freeside> question. I have a list of string infixes that i want to find in a string: ["/",":",".."] `areInfixesOf` "/some/file/path". if i were
--                   working against a list of filepaths, I could isInfixOf <$> infixlist <*> pathlist. But is there some idiomatic way to just search
--                   against a single filepath rather than a list of filepaths? hoogle returns "liftOp" for a type search for (a -> b -> c) -> [a] -> b ->
--                   [c]
-- 17:44 < freeside> I plan to Data.List.or the [c] which is [Bool] to see if any of the infixes matched
-- 17:46 < Solonarv> > map (\i -> i `isInfixOf` "/some/file/path") ["/",":",".."]
-- 17:46 < lambdabot>  [True,False,False]
-- 17:46 < freeside> is there some idiomatic way to do it without the lambda?
-- 17:46 < freeside> some sort of punctuation inside < >
-- 17:46 < Solonarv> yes, using an operator section:
-- 17:46 < Solonarv> > any (`isInfixOf` "/some/file/path") ["/",":",".."]
-- 17:46 < lambdabot>  True
-- 17:47 < freeside> ah, thank you
-- 17:47 < Solonarv> note: any f xs = or (map f xs)
-- 17:49 < freeside> what if i want to move things around so that the "/some/file/path" would sit at the rightmost end of the function definition? i believe
--                   i am groping toward something like point-free style
-- 17:49 < [Leary]> > or $ isInfixOf <$> ["/",":",".."] <*> pure "/some/file/path"
-- 17:49 < lambdabot>  True
-- 17:49 < Solonarv> @pl any (`isInfixOf` "/some/file/path") ["/",":",".."]
-- 17:50 < lambdabot> any (`isInfixOf` "/some/file/path") ["/", ":", ".."]
-- 17:50 < Solonarv> oh, right
-- 17:50 < Solonarv> @pl \needles hay -> any (`isInfixOf` hay) needles
-- 17:50 < lambdabot> flip (any . flip isInfixOf)
-- 17:50 < freeside> so the pure squishes my single argument into an invisible list so we can use <*>
-- 17:50 < Solonarv> :D
-- 17:51 < Solonarv> not an "invisible list", just a one-element list
-- 17:51 < Solonarv> pure "path" here is the same as ["path"]
-- 17:51 < freeside> gotcha
-- 17:52 < kuribas> freeside: pointless isn't idiomatic
-- 17:52 < freeside> i was poking around and found (??) in Lens
-- 17:52 < Solonarv> :t (??)
-- 17:53 < lambdabot> Functor f => f (a -> b) -> a -> f b
-- 17:53 < kuribas> freeside: and never use the function (reader) monad, unless you want to obfuscate
-- 17:53 < kuribas> deliberately
