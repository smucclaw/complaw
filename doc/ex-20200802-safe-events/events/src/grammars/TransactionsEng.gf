concrete TransactionsEng of Transactions = open
  Prelude,
  AdjectiveEng,
  SyntaxEng,
  ParamX,
  (R=ResEng),
  ParadigmsEng in {

  lincat
    -- LinKind and LinTerm are defined later in this file
    Kind = LinKind ;
    Term = LinTerm ;
    Determiner = LinDet ;
    Conjunction = LinConj ;
    [Term] = ListLinTerm ;
    Attribute = LinProp ;
    [Attribute] = ListLinProp ;

    -- Following categories on the RHS (Text, QS etc.) come from SyntaxEng
    Sentence = Text ;


  linref
    -- To make discontinuous categories show properly in the shell
    Kind = \x -> (mkUtt (merge x)).s ;
    Term = \x -> (mkUtt (np x)).s ;

  lin
    -- : Kind -> Attribute -> Move ;     -- liquidity event is voluntary
    SDefProp kind prop = mkText (mkUtt (mkCl (defTerm kind) (ap prop))) fullStopPunct ;

    -- : Kind -> Term -> Move ;         -- liquidity event means A, B or C
    SDefTerm kind term = mkText (mkUtt (mkCl (defTerm kind) (mkV2 (mkV "mean")) (np term))) fullStopPunct ;

    -- Determiners
    ASg = table {
      Mass => emptyDet ;
      Count => aSg_Det ;
      Plural => aPl_Det
      } ;
    APl = table {
      Mass => aPl_Det ; -- or emptyDet ;
      Count => aPl_Det ;
      Plural => aPl_Det
      } ;
    TheSg = table {
      Mass => theSg_Det ;
      Count => theSg_Det ;
      Plural => thePl_Det
      } ;
    ThePl = table {
      Mass => thePl_Det ;
      Count => thePl_Det ;
      Plural => thePl_Det
      } ;
    Any = table {
      Mass => anySg_Det ;
      Count => anySg_Det ;
      Plural => anyPl_Det
      } ;
    All = table {
      Mass => allSg_Det ;
      Count => all_Det ;
      Plural => all_Det
      } ;

    -- Kinds, Terms and Properties
    -- : Determiner -> Kind -> Term
    TDet = term ; -- using our oper 'term', defined at the end of file

    -- : Attribute -> Kind -> Kind ;
    KAttribute props kind = let prop : AP = ap props in
      case prop.isPre of {
        True => kind ** { -- voluntary termination
          cn = mkCN prop kind.cn
          } ;
        False => kind ** { -- termination for the benefit of the Company
          adv = cc2 kind.adv (ap2adv prop)
          }
      } ;

    -- : Attribute -> Attribute ;
    AttrNeg prop = table {
      --Neg => prop ! Pos  -- double negation = positive
      _ => prop ! Neg
      } ;

    -- Conjunctions
    And = and_Conj ;
    Or = or_Conj ;

    -- Lists
    -- The opers base and cons are defined later in this file.
    -- The instances of mkListAP can be found at
    --   https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListAP
    BaseAttribute = base AP ListAP mkListAP ; -- : AP -> AP -> ListAP
    ConsAttribute = cons AP ListAP mkListAP ; -- : AP -> ListAP -> ListAP
    ConjAttribute co ps = \\pol =>            -- : Conj -> ListAP -> AP
      mkAP co (ps ! pol) ; -- conjunctions don't change, because negations can be lexical.
                           -- e.g. "involuntary and unjustified"

    -- The instances of mkListNP can be found at
    --   https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListNP
    BaseTerm = mkListNP ; -- : NP -> NP -> ListNP ;
    ConsTerm = mkListNP ; -- : NP -> ListNP -> ListNP ;
    ConjTerm = mkNP ;     -- : Conj -> ListNP -> NP

  -----------------------------------------------------------------

  param
    KType = Mass | Count | Plural ;

  oper
    --------------------
    -- Types for lincats

    -- We don't need the full category of Det from RGL
    DetLite : Type = {s : Str ;  n : ParamX.Number} ;

    LinKind : Type = {
      cn : CN ;
      adv : Adv ;
      k : KType
      } ;
    LinTerm : Type = NP ;
    LinConj : Type = Conj ;
    LinDet : Type = KType => DetLite ;
    LinProp : Type = ParamX.Polarity => AP ;  -- Simplification: later use https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/YAQLFunctor.gf#L19

    --------
    -- Lists
    ListLinTerm : Type = ListNP ;
    ListLinProp : Type = ParamX.Polarity => ListAP ;

    -- This is how you do polymorphism in GF.
    -- A bit unwieldy.

    base : (A : Type) -> (ListA : Type) ->
      (A -> A -> ListA) ->
      (a, b : ParamX.Polarity => A) ->
      ParamX.Polarity => ListA =
      \_,_,mkList,a,b -> \\pol => mkList (a ! pol) (b ! pol) ;

    cons : (A : Type) -> (ListA : Type) ->
      (A -> ListA -> ListA) ->
      (a : ParamX.Polarity => A) ->
      (as : ParamX.Polarity => ListA) ->
      ParamX.Polarity => ListA =
      \_,_,mkList,a,as -> \\pol => mkList (a ! pol) (as ! pol) ;


    ------------
    -- Conj, Det

    neither7nor_DConj : Conj = mkConj "neither" "nor" singular ;

    -- Resource Grammar Library doesn't have any_Det, so we make it ourselves.
    -- Determiners are supposed to be closed class, so the constructor isn't
    -- exported in the API. (Silly, if you ask me.)
    -- The options are: open a low-level module and use the hidden constructor, or do this hack.
    anySg_Det : Det = a_Det ** { -- Extend a_Det: keyword ** is record extension
      s = "any"                -- Keep other fields from a_Det, but replace s with "any"
      } ;
    anyPl_Det : Det = aPl_Det ** {
      s = "any"
      } ;
    all_Det : Det = aPl_Det ** {
      s = "all"
      } ;
    allSg_Det : Det = a_Det ** {
      s = "all"
      } ;
    emptyDet : Det = a_Det ** {
      s = []
      } ;

    ----------------
    -- Empty phrases

    emptyAdv : Adv = mkAdv [] ;
    emptyAP : AP = <mkAP (mkA []) : AP> ** {s = \\_ => []} ;
    emptyNP : NP = <mkNP (mkN []) : NP> ** {s = \\_ => []} ;

    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ;  -- RGL has no AP->Adv fun
    adv2ap : Adv -> AP = AdjectiveEng.AdvAP emptyAP ; -- RGL has no Adv->AP fun

    -------------------------------
    -- Other useful syntactic opers

    -- Combine Determiner and Kind into a Term
    term : LinDet -> LinKind -> LinTerm = \dets,kind ->
      detCNLite (dets ! kind.k) (merge kind) ;

    defTerm : LinKind -> NP = \k -> mkNP (merge k) ;

    -- Merge the discontinuous Kind into a single CN
    merge : LinKind -> CN = \kind -> mkCN kind.cn kind.adv ;

    -- Default use for NPs
    np : LinTerm -> NP = id NP ;

    -- Default use for most APs: pick the positive version (e.g. "voluntary", not "involuntary")
    ap : LinProp -> AP = \lp -> lp ! Pos ;

    -- np2cn : NP -> CN = \np -> let s : Str = (mkUtt np).s in mkCN (mkN s s s s) ;

    -- copied from RGL to work with DetLite
    detCNLite : DetLite -> CN -> NP = \det,cn -> lin NP {
      s = \\c => det.s ++ cn.s ! det.n ! R.npcase2case c ;
      a = R.agrgP3 det.n cn.g
      } ;
}
