{-# LANGUAGE DeriveDataTypeable #-}
module SAFE where

import Data.Data
import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving (Show,Data)

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving (Show,Data)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving (Show,Data)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data Action =
   AComplDir ActionDir Term 
 | AComplIndir ActionIndir Term 
 | AComplNoneDir ActionDir ListTerm 
 | AComplNoneIndir ActionIndir ListTerm 
 | ANeg Action 
 | ConjAction Conjunction ListAction 
  deriving (Show,Data)

data ActionDir =
   ASlashIndir ActionDirIndir Term 
 | ConjSlashDir Conjunction ListActionDir 
 | Issue 
 | Raise 
 | Sell 
  deriving (Show,Data)

data ActionDirIndir =
   ConjSlashDirIndir Conjunction ListActionDirIndir 
 | IssueAt 
 | SellAt 
  deriving (Show,Data)

data ActionIndir =
   ASlashDir ActionDirIndir Term 
 | ConjSlashIndir Conjunction ListActionIndir 
 | PursuantTo Action 
  deriving (Show,Data)

data Attribute =
   AttrNeg Attribute 
 | BonaFide 
 | ConjAttribute Conjunction ListAttribute 
 | Fixed 
 | ForBenefit Term 
 | PostMoney 
 | PreMoney 
 | Voluntary 
 | WithPurpose Action 
  deriving (Show,Data)

data Conjunction =
   And 
 | Or 
  deriving (Show,Data)

data Determiner =
   APl 
 | ASg 
 | All 
 | Any 
 | AnyOther 
 | ThePl 
 | TheSg 
  deriving (Show,Data)

data Event = Ev Kind 
  deriving (Show,Data)

data Kind =
   Capital 
 | ChangeOfControl 
 | ComplKind KindTerm Term 
 | DirectListing 
 | DissolutionEvent 
 | EquityFinancing 
 | GeneralAssignment 
 | InitialPublicOffering 
 | KAttribute Attribute Kind 
 | KWhetherOr ListAttribute Kind 
 | LiquidityEvent 
 | PreferredStock 
 | SingleOrSeries Kind 
 | Termination 
 | Transaction 
 | Valuation 
  deriving (Show,Data)

data KindTerm =
   ConjSlashTerm Conjunction ListKindTerm 
 | Dissolution 
 | Liquidation 
 | WindingUp 
  deriving (Show,Data)

newtype ListAction = ListAction [Action] deriving (Show,Data)

newtype ListActionDir = ListActionDir [ActionDir] deriving (Show,Data)

newtype ListActionDirIndir = ListActionDirIndir [ActionDirIndir] deriving (Show,Data)

newtype ListActionIndir = ListActionIndir [ActionIndir] deriving (Show,Data)

newtype ListAttribute = ListAttribute [Attribute] deriving (Show,Data)

newtype ListKind = ListKind [Kind] deriving (Show,Data)

newtype ListKindTerm = ListKindTerm [KindTerm] deriving (Show,Data)

newtype ListTerm = ListTerm [Term] deriving (Show,Data)

data Sentence =
   SAction Temporal Term Action 
 | SDefProp Kind Attribute 
 | SDefTerm Kind Term 
  deriving (Show,Data)

data Temporal =
   TFuture 
 | TPast 
 | TPresent 
 | Upon Event 
  deriving (Show,Data)

data Term =
   Company 
 | ConjTerm Conjunction ListTerm 
 | Creditors Term 
 | RelDir Term Term Temporal ActionDir 
 | RelIndir Term Term Temporal ActionIndir 
 | TDet Determiner Kind 
 | TExcluding Determiner Kind Term 
 | TIncluding Determiner Kind Term 
  deriving (Show,Data)

data Agreement

data Clause


instance Gf Action where
  gf (AComplDir x1 x2) = mkApp (mkCId "AComplDir") [gf x1, gf x2]
  gf (AComplIndir x1 x2) = mkApp (mkCId "AComplIndir") [gf x1, gf x2]
  gf (AComplNoneDir x1 x2) = mkApp (mkCId "AComplNoneDir") [gf x1, gf x2]
  gf (AComplNoneIndir x1 x2) = mkApp (mkCId "AComplNoneIndir") [gf x1, gf x2]
  gf (ANeg x1) = mkApp (mkCId "ANeg") [gf x1]
  gf (ConjAction x1 x2) = mkApp (mkCId "ConjAction") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AComplDir" -> AComplDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplIndir" -> AComplIndir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplNoneDir" -> AComplNoneDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AComplNoneIndir" -> AComplNoneIndir (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ANeg" -> ANeg (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ConjAction" -> ConjAction (fg x1) (fg x2)


      _ -> error ("no Action " ++ show t)

instance Gf ActionDir where
  gf (ASlashIndir x1 x2) = mkApp (mkCId "ASlashIndir") [gf x1, gf x2]
  gf (ConjSlashDir x1 x2) = mkApp (mkCId "ConjSlashDir") [gf x1, gf x2]
  gf Issue = mkApp (mkCId "Issue") []
  gf Raise = mkApp (mkCId "Raise") []
  gf Sell = mkApp (mkCId "Sell") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ASlashIndir" -> ASlashIndir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjSlashDir" -> ConjSlashDir (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Issue" -> Issue 
      Just (i,[]) | i == mkCId "Raise" -> Raise 
      Just (i,[]) | i == mkCId "Sell" -> Sell 


      _ -> error ("no ActionDir " ++ show t)

instance Gf ActionDirIndir where
  gf (ConjSlashDirIndir x1 x2) = mkApp (mkCId "ConjSlashDirIndir") [gf x1, gf x2]
  gf IssueAt = mkApp (mkCId "IssueAt") []
  gf SellAt = mkApp (mkCId "SellAt") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjSlashDirIndir" -> ConjSlashDirIndir (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "IssueAt" -> IssueAt 
      Just (i,[]) | i == mkCId "SellAt" -> SellAt 


      _ -> error ("no ActionDirIndir " ++ show t)

instance Gf ActionIndir where
  gf (ASlashDir x1 x2) = mkApp (mkCId "ASlashDir") [gf x1, gf x2]
  gf (ConjSlashIndir x1 x2) = mkApp (mkCId "ConjSlashIndir") [gf x1, gf x2]
  gf (PursuantTo x1) = mkApp (mkCId "PursuantTo") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ASlashDir" -> ASlashDir (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjSlashIndir" -> ConjSlashIndir (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PursuantTo" -> PursuantTo (fg x1)


      _ -> error ("no ActionIndir " ++ show t)

instance Gf Attribute where
  gf (AttrNeg x1) = mkApp (mkCId "AttrNeg") [gf x1]
  gf BonaFide = mkApp (mkCId "BonaFide") []
  gf (ConjAttribute x1 x2) = mkApp (mkCId "ConjAttribute") [gf x1, gf x2]
  gf Fixed = mkApp (mkCId "Fixed") []
  gf (ForBenefit x1) = mkApp (mkCId "ForBenefit") [gf x1]
  gf PostMoney = mkApp (mkCId "PostMoney") []
  gf PreMoney = mkApp (mkCId "PreMoney") []
  gf Voluntary = mkApp (mkCId "Voluntary") []
  gf (WithPurpose x1) = mkApp (mkCId "WithPurpose") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AttrNeg" -> AttrNeg (fg x1)
      Just (i,[]) | i == mkCId "BonaFide" -> BonaFide 
      Just (i,[x1,x2]) | i == mkCId "ConjAttribute" -> ConjAttribute (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Fixed" -> Fixed 
      Just (i,[x1]) | i == mkCId "ForBenefit" -> ForBenefit (fg x1)
      Just (i,[]) | i == mkCId "PostMoney" -> PostMoney 
      Just (i,[]) | i == mkCId "PreMoney" -> PreMoney 
      Just (i,[]) | i == mkCId "Voluntary" -> Voluntary 
      Just (i,[x1]) | i == mkCId "WithPurpose" -> WithPurpose (fg x1)


      _ -> error ("no Attribute " ++ show t)

instance Gf Conjunction where
  gf And = mkApp (mkCId "And") []
  gf Or = mkApp (mkCId "Or") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> And 
      Just (i,[]) | i == mkCId "Or" -> Or 


      _ -> error ("no Conjunction " ++ show t)

instance Gf Determiner where
  gf APl = mkApp (mkCId "APl") []
  gf ASg = mkApp (mkCId "ASg") []
  gf All = mkApp (mkCId "All") []
  gf Any = mkApp (mkCId "Any") []
  gf AnyOther = mkApp (mkCId "AnyOther") []
  gf ThePl = mkApp (mkCId "ThePl") []
  gf TheSg = mkApp (mkCId "TheSg") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "APl" -> APl 
      Just (i,[]) | i == mkCId "ASg" -> ASg 
      Just (i,[]) | i == mkCId "All" -> All 
      Just (i,[]) | i == mkCId "Any" -> Any 
      Just (i,[]) | i == mkCId "AnyOther" -> AnyOther 
      Just (i,[]) | i == mkCId "ThePl" -> ThePl 
      Just (i,[]) | i == mkCId "TheSg" -> TheSg 


      _ -> error ("no Determiner " ++ show t)

instance Gf Event where
  gf (Ev x1) = mkApp (mkCId "Ev") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Ev" -> Ev (fg x1)


      _ -> error ("no Event " ++ show t)

instance Gf Kind where
  gf Capital = mkApp (mkCId "Capital") []
  gf ChangeOfControl = mkApp (mkCId "ChangeOfControl") []
  gf (ComplKind x1 x2) = mkApp (mkCId "ComplKind") [gf x1, gf x2]
  gf DirectListing = mkApp (mkCId "DirectListing") []
  gf DissolutionEvent = mkApp (mkCId "DissolutionEvent") []
  gf EquityFinancing = mkApp (mkCId "EquityFinancing") []
  gf GeneralAssignment = mkApp (mkCId "GeneralAssignment") []
  gf InitialPublicOffering = mkApp (mkCId "InitialPublicOffering") []
  gf (KAttribute x1 x2) = mkApp (mkCId "KAttribute") [gf x1, gf x2]
  gf (KWhetherOr x1 x2) = mkApp (mkCId "KWhetherOr") [gf x1, gf x2]
  gf LiquidityEvent = mkApp (mkCId "LiquidityEvent") []
  gf PreferredStock = mkApp (mkCId "PreferredStock") []
  gf (SingleOrSeries x1) = mkApp (mkCId "SingleOrSeries") [gf x1]
  gf Termination = mkApp (mkCId "Termination") []
  gf Transaction = mkApp (mkCId "Transaction") []
  gf Valuation = mkApp (mkCId "Valuation") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Capital" -> Capital 
      Just (i,[]) | i == mkCId "ChangeOfControl" -> ChangeOfControl 
      Just (i,[x1,x2]) | i == mkCId "ComplKind" -> ComplKind (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "DirectListing" -> DirectListing 
      Just (i,[]) | i == mkCId "DissolutionEvent" -> DissolutionEvent 
      Just (i,[]) | i == mkCId "EquityFinancing" -> EquityFinancing 
      Just (i,[]) | i == mkCId "GeneralAssignment" -> GeneralAssignment 
      Just (i,[]) | i == mkCId "InitialPublicOffering" -> InitialPublicOffering 
      Just (i,[x1,x2]) | i == mkCId "KAttribute" -> KAttribute (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "KWhetherOr" -> KWhetherOr (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "LiquidityEvent" -> LiquidityEvent 
      Just (i,[]) | i == mkCId "PreferredStock" -> PreferredStock 
      Just (i,[x1]) | i == mkCId "SingleOrSeries" -> SingleOrSeries (fg x1)
      Just (i,[]) | i == mkCId "Termination" -> Termination 
      Just (i,[]) | i == mkCId "Transaction" -> Transaction 
      Just (i,[]) | i == mkCId "Valuation" -> Valuation 


      _ -> error ("no Kind " ++ show t)

instance Gf KindTerm where
  gf (ConjSlashTerm x1 x2) = mkApp (mkCId "ConjSlashTerm") [gf x1, gf x2]
  gf Dissolution = mkApp (mkCId "Dissolution") []
  gf Liquidation = mkApp (mkCId "Liquidation") []
  gf WindingUp = mkApp (mkCId "WindingUp") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjSlashTerm" -> ConjSlashTerm (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Dissolution" -> Dissolution 
      Just (i,[]) | i == mkCId "Liquidation" -> Liquidation 
      Just (i,[]) | i == mkCId "WindingUp" -> WindingUp 


      _ -> error ("no KindTerm " ++ show t)

instance Gf ListAction where
  gf (ListAction [x1,x2]) = mkApp (mkCId "BaseAction") [gf x1, gf x2]
  gf (ListAction (x:xs)) = mkApp (mkCId "ConsAction") [gf x, gf (ListAction xs)]
  fg t =
    ListAction (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAction" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAction" -> fg x1 : fgs x2


      _ -> error ("no ListAction " ++ show t)

instance Gf ListActionDir where
  gf (ListActionDir [x1,x2]) = mkApp (mkCId "BaseActionDir") [gf x1, gf x2]
  gf (ListActionDir (x:xs)) = mkApp (mkCId "ConsActionDir") [gf x, gf (ListActionDir xs)]
  fg t =
    ListActionDir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseActionDir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsActionDir" -> fg x1 : fgs x2


      _ -> error ("no ListActionDir " ++ show t)

instance Gf ListActionDirIndir where
  gf (ListActionDirIndir [x1,x2]) = mkApp (mkCId "BaseActionDirIndir") [gf x1, gf x2]
  gf (ListActionDirIndir (x:xs)) = mkApp (mkCId "ConsActionDirIndir") [gf x, gf (ListActionDirIndir xs)]
  fg t =
    ListActionDirIndir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseActionDirIndir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsActionDirIndir" -> fg x1 : fgs x2


      _ -> error ("no ListActionDirIndir " ++ show t)

instance Gf ListActionIndir where
  gf (ListActionIndir [x1,x2]) = mkApp (mkCId "BaseActionIndir") [gf x1, gf x2]
  gf (ListActionIndir (x:xs)) = mkApp (mkCId "ConsActionIndir") [gf x, gf (ListActionIndir xs)]
  fg t =
    ListActionIndir (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseActionIndir" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsActionIndir" -> fg x1 : fgs x2


      _ -> error ("no ListActionIndir " ++ show t)

instance Gf ListAttribute where
  gf (ListAttribute [x1,x2]) = mkApp (mkCId "BaseAttribute") [gf x1, gf x2]
  gf (ListAttribute (x:xs)) = mkApp (mkCId "ConsAttribute") [gf x, gf (ListAttribute xs)]
  fg t =
    ListAttribute (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAttribute" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAttribute" -> fg x1 : fgs x2


      _ -> error ("no ListAttribute " ++ show t)

instance Gf ListKind where
  gf (ListKind [x1,x2]) = mkApp (mkCId "BaseKind") [gf x1, gf x2]
  gf (ListKind (x:xs)) = mkApp (mkCId "ConsKind") [gf x, gf (ListKind xs)]
  fg t =
    ListKind (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseKind" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsKind" -> fg x1 : fgs x2


      _ -> error ("no ListKind " ++ show t)

instance Gf ListKindTerm where
  gf (ListKindTerm [x1,x2]) = mkApp (mkCId "BaseKindTerm") [gf x1, gf x2]
  gf (ListKindTerm (x:xs)) = mkApp (mkCId "ConsKindTerm") [gf x, gf (ListKindTerm xs)]
  fg t =
    ListKindTerm (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseKindTerm" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsKindTerm" -> fg x1 : fgs x2


      _ -> error ("no ListKindTerm " ++ show t)

instance Gf ListTerm where
  gf (ListTerm [x1,x2]) = mkApp (mkCId "BaseTerm") [gf x1, gf x2]
  gf (ListTerm (x:xs)) = mkApp (mkCId "ConsTerm") [gf x, gf (ListTerm xs)]
  fg t =
    ListTerm (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseTerm" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsTerm" -> fg x1 : fgs x2


      _ -> error ("no ListTerm " ++ show t)

instance Gf Sentence where
  gf (SAction x1 x2 x3) = mkApp (mkCId "SAction") [gf x1, gf x2, gf x3]
  gf (SDefProp x1 x2) = mkApp (mkCId "SDefProp") [gf x1, gf x2]
  gf (SDefTerm x1 x2) = mkApp (mkCId "SDefTerm") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "SAction" -> SAction (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SDefProp" -> SDefProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SDefTerm" -> SDefTerm (fg x1) (fg x2)


      _ -> error ("no Sentence " ++ show t)

instance Gf Temporal where
  gf TFuture = mkApp (mkCId "TFuture") []
  gf TPast = mkApp (mkCId "TPast") []
  gf TPresent = mkApp (mkCId "TPresent") []
  gf (Upon x1) = mkApp (mkCId "Upon") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "TFuture" -> TFuture 
      Just (i,[]) | i == mkCId "TPast" -> TPast 
      Just (i,[]) | i == mkCId "TPresent" -> TPresent 
      Just (i,[x1]) | i == mkCId "Upon" -> Upon (fg x1)


      _ -> error ("no Temporal " ++ show t)

instance Gf Term where
  gf Company = mkApp (mkCId "Company") []
  gf (ConjTerm x1 x2) = mkApp (mkCId "ConjTerm") [gf x1, gf x2]
  gf (Creditors x1) = mkApp (mkCId "Creditors") [gf x1]
  gf (RelDir x1 x2 x3 x4) = mkApp (mkCId "RelDir") [gf x1, gf x2, gf x3, gf x4]
  gf (RelIndir x1 x2 x3 x4) = mkApp (mkCId "RelIndir") [gf x1, gf x2, gf x3, gf x4]
  gf (TDet x1 x2) = mkApp (mkCId "TDet") [gf x1, gf x2]
  gf (TExcluding x1 x2 x3) = mkApp (mkCId "TExcluding") [gf x1, gf x2, gf x3]
  gf (TIncluding x1 x2 x3) = mkApp (mkCId "TIncluding") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Company" -> Company 
      Just (i,[x1,x2]) | i == mkCId "ConjTerm" -> ConjTerm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Creditors" -> Creditors (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "RelDir" -> RelDir (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "RelIndir" -> RelIndir (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "TDet" -> TDet (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "TExcluding" -> TExcluding (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "TIncluding" -> TIncluding (fg x1) (fg x2) (fg x3)


      _ -> error ("no Term " ++ show t)

instance Show Agreement

instance Gf Agreement where
  gf _ = undefined
  fg _ = undefined



instance Show Clause

instance Gf Clause where
  gf _ = undefined
  fg _ = undefined




