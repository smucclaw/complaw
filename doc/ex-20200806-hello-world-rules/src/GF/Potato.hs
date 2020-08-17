module GF.Potato where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

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
   Plant Object
 | Trade Object
  deriving Show

data Modal =
   May
 | Must
 | Shant
  deriving Show

data Object =
   Cabbage
 | Potato
  deriving Show

data Party =
   Everybody
 | MkParty GString
 | Nobody
  deriving Show

data Rule = MkRule Party Modal Action
  deriving Show


instance Gf Action where
  gf (Plant x1) = mkApp (mkCId "Plant") [gf x1]
  gf (Trade x1) = mkApp (mkCId "Trade") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Plant" -> Plant (fg x1)
      Just (i,[x1]) | i == mkCId "Trade" -> Trade (fg x1)


      _ -> error ("no Action " ++ show t)

instance Gf Modal where
  gf May = mkApp (mkCId "May") []
  gf Must = mkApp (mkCId "Must") []
  gf Shant = mkApp (mkCId "Shant") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "May" -> May
      Just (i,[]) | i == mkCId "Must" -> Must
      Just (i,[]) | i == mkCId "Shant" -> Shant


      _ -> error ("no Modal " ++ show t)

instance Gf Object where
  gf Cabbage = mkApp (mkCId "Cabbage") []
  gf Potato = mkApp (mkCId "Potato") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Cabbage" -> Cabbage
      Just (i,[]) | i == mkCId "Potato" -> Potato


      _ -> error ("no Object " ++ show t)

instance Gf Party where
  gf Everybody = mkApp (mkCId "Everybody") []
  gf (MkParty x1) = mkApp (mkCId "MkParty") [gf x1]
  gf Nobody = mkApp (mkCId "Nobody") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Everybody" -> Everybody
      Just (i,[x1]) | i == mkCId "MkParty" -> MkParty (fg x1)
      Just (i,[]) | i == mkCId "Nobody" -> Nobody


      _ -> error ("no Party " ++ show t)

instance Gf Rule where
  gf (MkRule x1 x2 x3) = mkApp (mkCId "MkRule") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkRule" -> MkRule (fg x1) (fg x2) (fg x3)


      _ -> error ("no Rule " ++ show t)
