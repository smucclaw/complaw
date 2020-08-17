-- DO NOT EDIT THIS FILE!
-- direct edits will be clobbered.
--
-- this file is autogenerated!
-- open the parent README.org in emacs and hit   C-c C-v t   to regenerate this file.

module GF.Paraphrase where
import PGF hiding (paraphrase)
import GF.Potato

paraphrase :: Rule -> Rule
paraphrase (MkRule Nobody May action) = MkRule Everybody Shant action
paraphrase (MkRule Everybody Shant action) = MkRule Nobody May action
paraphrase rule = rule

paraphraseExpr :: Expr -> Expr
paraphraseExpr = gf . paraphrase . fg

paraphraseString :: PGF -> String -> String
paraphraseString gr str = linearize gr eng (paraphraseExpr tree)
  where
    eng = head $ languages gr  :: Language
    rule = startCat gr         :: Type
    tree = head $ parse gr eng rule str :: Expr
