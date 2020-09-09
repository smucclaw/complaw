module L4Rules where

import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybe)
import Text.PrettyPrint as PP




class MyPretty a where
  mypretty :: a -> Doc

-- have to rework this -- making a mild category error. how do we talk about a party in the abstract, when we don't care about which constructor it is? for now let's use SomeParty
data Party = NaturalPerson { name :: String }
           | Corporation   { name :: String }
           | Trust         { name :: String }
           | SomeParty     { name :: String }
           deriving (Show, Eq)

instance MyPretty Party where
  mypretty (NaturalPerson name) = text name PP.<> comma <+> text "a natural person"
  mypretty (Corporation   name) = text name PP.<> comma <+> text "a corporation"
  mypretty (Trust         name) = text name PP.<> comma <+> text "a trust"

-- EDSL / AST

data RuleSpec = RuleGroupSpec L4RuleGroup
              | OneRuleSpec L4Rule
              | RuleSpecExp BlockExp
  deriving (Show, Eq)

  -- the RuleSpecExp allows us to say "notwithstanding any other rule in this section which would have the effect of reducing the severity of the penalty described herein"

data L4RuleGroup = RuleGroup { groupTitle :: RuleTitle
                             , ruleTitles :: [RuleTitle]  }
  deriving (Show, Eq)

data L4DefeasibleRelation = L4RuleDefeasible { subjectTo :: [RuleSpec]
                                             , notwithstanding :: [RuleSpec]
                                             , rule :: RuleTitle }
  deriving (Show, Eq)

-- this will turn into a graph one day
data RuleSequenceRelation = RuleHence | RuleLest -- used for violation and reparation
  deriving (Show, Eq)

type RuleTitle = String

-- L4 statement generally, not a rule
data L4Stm = Def Keyword DefType StmFormula

type Keyword = String -- should make this a NonSpaceString

data DefType = DeonticKeyword
             | ConstitutiveKeyword

type StmFormula = BlockExp

-- this allows us to, e.g., define strong and weak permission, and define SHANT in terms of NOT MAY

-- for types of rules, see 
data L4Rule = DefinitionRule { title :: RuleTitle
                             , annotations :: [RuleAnnotations]
                             , aliases     :: Maybe [String]
                             , forall :: Object
                             , block :: BlockExp
                             }
            | ConstitutiveRule { title      :: RuleTitle
                               , annotations :: [RuleAnnotations]
                               , aliases     :: Maybe [String]
                               , scope       :: Maybe [RuleSpec]
                               , forall      :: Object -- context / scope / with
                               , x          :: Object
                               , y          :: Object
                               , conditions :: BoolExp
                               }
            | RegulativeRule { title       :: RuleTitle
                             , annotations :: [RuleAnnotations]
                             , aliases     :: Maybe [String]
                             , conditions  :: BoolExp
                             , party       :: Party
                             , deontic     :: Deontic
                             , action      :: Action
                             , temporal    :: Temporal
                             , hence       :: RuleFollowsHence -- sub-state-machine
                             , lest        :: RuleFollowsLest
                          }
            | MetaRule
            | CompoundRule [L4Rule]
            | ConstraintRule
  deriving (Show, Eq)

data RuleFollowsHence = HenceFulfilled | Hence RuleTitle | NoHence
  deriving (Show, Eq)

data RuleFollowsLest = LestBreach | Lest RuleTitle | NoLest
  deriving (Show, Eq)

data Deontic = Must | Should | May | Shant
  deriving (Show, Eq)

instance MyPretty Deontic where
  mypretty Must   = text "MUST"
  mypretty Should = text "SHOULD"
  mypretty May    = text "MAY"
  mypretty Shant  = text "SHANT"

instance MyPretty L4Rule where

  mypretty l4r@(DefinitionRule t annotations aliases f blexp) =
    rulePrefix l4r <+> text "DEFINITION RULE" <+> doubleQuotes (text t) $+$
    text "FORALL" <+> mypretty f <+>
    lbrace $+$ nest 2 ( mypretty blexp ) $+$ rbrace

  mypretty l4r@(RegulativeRule title annotations aliases conditions party deontic action temporal hence lest) =
    vcat [ rulePrefix l4r <+> text "REGULATIVE RULE" <+> doubleQuotes (text title)
         , (if conditions /= (BBool True) then text "under certain conditions" <+> lbrack <+> nest 2 (mypretty conditions) <+> rbrack else empty)
         , text "PARTY" <+> mypretty party
         , hsep [ (if PassiveVoice `elem` annotations then text "P" else empty) PP.<> mypretty deontic,
                  mypretty action ]
         , mypretty temporal
         ]

  mypretty l4r@(ConstitutiveRule title annotations aliases scope forall x y conditions) =
    vcat [ rulePrefix l4r <+> text "CONSTITUTIVE RULE" <+> doubleQuotes (text title)
         , text "FORALL" <+> mypretty forall <+> lbrace
         , nest 2 $ vcat [ mypretty x <+> text ":-" <+> myprolog conditions ]
         , rbrace
         ]


rulePrefix l4rule = hcat [ if Unspoken `elem` (annotations l4rule) then text "UNSPOKEN" else empty ]

-- wouldn't it be cool if we could do some CTL here.
-- also, wouldn't it be cool if there were some time library we could reuse

data Temporal = Temporal (Maybe TemporalInt) (Maybe TemporalBase) Repetition
              | TemporalSpec BlockExp
  deriving (Show, Eq)

instance MyPretty Temporal where
  mypretty (TemporalSpec ble) = text "according to the temporal specification" <+> mypretty ble
  mypretty (Temporal (Just interval) (Just base) repetition) =
    let base_interval = mypretty base $+$ mypretty interval
    in base_interval <+> mypretty repetition
  mypretty (Temporal Nothing Nothing repetition) = mypretty repetition


data TemporalBase = After     DateSpec -- >
                  | Before    DateSpec -- <
                  | On        DateSpec -- =
                  | OnOrAbout DateSpec -- ~=
  deriving (Show, Eq)

instance MyPretty TemporalBase where
  mypretty (After     datespec) = text "AFTER"  <+> mypretty datespec
  mypretty (Before    datespec) = text "BEFORE" <+> mypretty datespec
  mypretty (On        datespec) = text "ON"     <+> mypretty datespec
  mypretty (OnOrAbout datespec) = text "ABOUT"  <+> mypretty datespec



data TemporalInt = Within Interval
                 | IntervalSpec BlockExp
  deriving (Show, Eq)

instance MyPretty TemporalInt where
  mypretty (Within interval) = text "WITHIN" <+> mypretty interval
  mypretty (IntervalSpec ble) = text "WITHIN" <+> mypretty ble

data Repetition = Repeating (Maybe Interval)
                | Once
                | Times NumExp
  deriving (Show, Eq)

instance MyPretty Repetition where
  mypretty (Repeating Nothing) = text "REPEATEDLY every so often"
  mypretty (Repeating (Just interval)) = text "REPEATEDLY" <+> text "every" <+> mypretty interval
  mypretty Once                 = empty
  mypretty (Times numexp)       = mypretty numexp

data Interval = Interval (Maybe IYear) (Maybe IMonth) (Maybe IDay)
  deriving (Show, Eq)


instance MyPretty Interval where
  mypretty (Interval Nothing Nothing Nothing) = text "some time period not properly specified -- this is an error"
  mypretty (Interval y m d) = (sayperiod y "year") <+> (sayperiod m "month") <+> (sayperiod d "day")
    where
      sayperiod Nothing  str = empty
      sayperiod (Just n) str | n > 1  = text (show n) <+> text (str ++ "s")
                             | n == 1 = text (show n) <+> text (str)

type IYear  = Int
type IMonth = Int
type IDay   = Int

data NumExp = Fixed Int
            | NumSpec BlockExp
  deriving (Show, Eq)

instance MyPretty NumExp where
  mypretty (Fixed n) = text $ show n
  mypretty (NumSpec blexp) = text "according to the numerical specification" <+> mypretty blexp

data DateSpec = Gregorian Int Int Int -- Y M D
              | DateSpec BlockExp
  deriving (Show, Eq) -- would be nice to Ord the Gregorian

instance MyPretty DateSpec where
  mypretty (Gregorian y m d) = text "in the year of our lord" <+> text (show y) <+>
                               text "on the" <+> text (show d) <+> text "day of the " <+>
                               text (show m) <+> text "month"

data RuleAnnotations = Unspoken
                     | LegalSource String
                     | PassiveVoice
                     | SubjectTo [RuleSpec]
                     | Notwithstanding [RuleSpec]
  deriving (Show, Eq)

instance MyPretty RuleAnnotations where
  mypretty (Unspoken)           = text "UNSPOKEN"
  mypretty (LegalSource string) = text ("lrml:LegalSource" ++ string)
  mypretty (PassiveVoice)       = text ("passive voice")

type SymbolName = String

data Object = Object { objType :: ObjType
                     , objName :: SymbolName
                     }
  deriving (Show, Eq)

-- refactor data Object = ArrayObj [Object] | StringObj | EnumObj String String | SelectorObj String

data ObjType = TypeArray ObjType
             | TypeString
             | TypeEnum String
             | TypeSelector
             | TypeBool
  deriving (Show, Eq)

type Lang = String

type LangString = (Lang, String)

prettylangstring :: LangString -> Doc
prettylangstring (lang, string) = text (":" ++ lang ++ ":") <+> text string

-- to be expanded. perhaps we do event-calculus start/finish in the trace
data Action = Action Object [LangString] (Maybe BoolExp)
  deriving (Show, Eq)

instance MyPretty Action where
  mypretty (Action (Object otype oname) langstrings blexp) =
    vcat (text oname : (prettylangstring <$> langstrings)
          ++ [maybe empty (\bl -> text "WHERE" <+> mypretty bl) blexp]
         )

instance MyPretty Object where
  mypretty (Object otype oname) = text oname <+> (vpretty $ colon PP.<> colon PP.<> space PP.<> mypretty otype)

verbosity = 0
vpretty x = if verbosity > 0 then x else empty

instance MyPretty ObjType where
  mypretty (TypeArray otype) = lbrack PP.<> mypretty otype PP.<> rbrack
  mypretty (TypeBool)        = text "Bool"
  mypretty (TypeString)      = text "String"
  mypretty (TypeEnum str)    = text ("Enum of " ++ str)
  mypretty (TypeSelector)    = text "ObjectSelector"

-- block expressions which evaluate to Boolean
data BoolExp = BEquals BoolExp BoolExp
              | BlEq BlockExp BlockExp
              | BNot BoolExp
              | BAnd [BoolExp]
              | BAny [BoolExp]
              | BAll [BoolExp]
              | BMatch BlockExp BlockExp
              | BBool Bool
              | BlAsBool BlockExp
  deriving (Show, Eq)
-- need a monad transformer to equip BlockExp with a Reader context

data L4List a = L4List ListType [a]
  deriving (Show, Eq)

data BlockExp = BlockExp Object -- with explicit braces
              | BlUnify BlockExp BlockExp
              | BIfThenElse    BoolExp (Maybe BlockExp) (Maybe BlockExp)  -- explicit "if x then y else z" -- note the order of arguments is always x y z
              | BIfElse1       BoolExp (Maybe BlockExp) (Maybe BlockExp)  -- implicit "y if x; z otherwise"
              | BZunlessXthenY BoolExp (Maybe BlockExp) (Maybe BlockExp)  -- implicit "z unless x in which case y"
              | BlOf BlockExp BlockExp -- X of Y
              | BlObj Object
              | BlStr String
              | BlList (L4List BlockExp)
--            | BCase1 Object -- explicit "case x of y1 -> z1; y2 -> z2; y3 -> z3"
  deriving (Show, Eq)

data ListType = ConjAnd | DisjAnd | Xor | ConjOr | DisjOr
  deriving (Show, Eq)

data Direction = TopDown | BottomUp
  deriving (Show, Eq)


instance MyPretty BoolExp where
  mypretty (BEquals be1 be2)  = mypretty be1 <+> equals PP.<> equals <+> mypretty be2
  mypretty (BlEq ble1 ble2)   = mypretty ble1 <+> text "=~=" <+> mypretty ble2
  mypretty (BNot be)          = text "NOT" <+> lparen PP.<> mypretty be PP.<> rparen
  mypretty (BAnd bes)         = text "AND" <+> lbrack <+> hsep (punctuate comma (mypretty <$> bes)) <+> rbrack
  mypretty (BAny bes)         = text "ANY" <+> lbrack <+> hsep (punctuate comma (mypretty <$> bes)) <+> rbrack
  mypretty (BAll bes)         = text "ALL" <+> lbrack <+> hsep (punctuate comma (mypretty <$> bes)) <+> rbrack
  mypretty (BMatch ble1 ble2) = mypretty ble1 <+> text "~~" <+> mypretty ble2
  mypretty (BBool bool)       = text $ show bool
  mypretty (BlAsBool obj)     = mypretty obj

-- sometimes "any" means "all", as in "to be read with any subsequent amendment"

myprolog (BAnd bes)         = hsep (punctuate comma (mypretty <$> bes)) PP.<> text "."

instance MyPretty BlockExp where
  mypretty (BlObj    obj) = mypretty obj
  mypretty (BlockExp obj) = vcat [ lbrace, nest 2 (mypretty obj), rbrace ]
  mypretty (BlUnify   ble1 ble2) = mypretty ble1 <+> equals <+> mypretty ble2
  mypretty (BIfThenElse be1 ble2 ble3) = text "IF" <+> mypretty be1 $+$
                                         maybeBranch "THEN" ble2 $+$
                                         maybeBranch "ELSE" ble3
  mypretty (BIfElse1 be1 ble2 ble3) = mypretty ble2 <+>
                                      text "PROVIDED" <+> mypretty be1 $+$
                                      maybeBranch "OTHERWISE" ble3
  mypretty (BZunlessXthenY be1 ble2 Nothing) = mypretty (BIfElse1 be1 ble2 Nothing)
  mypretty (BZunlessXthenY be1 ble2 (Just ble3)) = mypretty ble3 $+$
                                                   text "UNLESS" <+> mypretty be1 $+$
                                                   maybeBranch "IN WHICH CASE" ble2
  mypretty (BlOf     ble1 ble2)     = mypretty ble1 <+> text "OF" <+> mypretty ble2
  mypretty (BlStr    s)   = doubleQuotes $ text s
  mypretty (BlList l4list) = mypretty l4list

instance (MyPretty a) => MyPretty (L4List a) where
  mypretty (L4List listtype as) = lbrack <+> oxford listtype (mypretty <$> as) <+> rbrack
    where
      oxford lt prettys = nest 2 (vcat $ punctuate comma (listbody prettys) ++ [text $ listsym lt] ++ [last prettys])
      listbody xs = take (length xs - 1) xs
      listsym DisjAnd = "&"
      listsym ConjAnd = "&&"
      listsym Xor     = "|"
      listsym DisjOr  = "||"
      listsym ConjOr  = "|||"

indent num ss = ((replicate num ' ') ++) <$> ss

maybeBranch str Nothing = empty
maybeBranch str justx   = text str <+> mypretty justx

instance (MyPretty x) => MyPretty (Maybe x) where
  mypretty Nothing = empty
  mypretty (Just a) = mypretty a
