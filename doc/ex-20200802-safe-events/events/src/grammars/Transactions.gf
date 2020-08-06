abstract Transactions = {
  flags startcat = Sentence ;
  cat
    Sentence ;

--    Transaction ;
    Clause ;
    Agreement ;

    Kind ;
    [Kind]{2} ;
    Attribute ;
    [Attribute]{2} ;
    Term ;
    [Term]{2} ;

    Determiner ;
    Conjunction ;

  fun

    -- Definitions
    SDefProp : Kind -> Attribute -> Sentence ;    -- liquidity event is voluntary
    SDefTerm : Kind -> Term -> Sentence ;         -- liquidity event means A, B or C

    -- Determiners
    ASg,                                     -- a post-money valuation
    APl,                                     -- creditors
    TheSg,                                   -- the company
    ThePl,                                   -- the companies
    All,                                     -- all dissolution events
    Any : Determiner ;                       -- any liquidation event

    TDet : Determiner -> Kind -> Term ;

    -- Kinds, Terms and Attributes
    AttrNeg : Attribute -> Attribute ;             -- not fixed / involuntary
    KAttribute : Attribute -> Kind -> Kind ;    -- voluntary termination

    -- Conjunctions
    Or,
    And : Conjunction ;

    ConjTerm                                  -- change of control or direct listing
      : Conjunction -> [Term] -> Term ;
    ConjAttribute
      : Conjunction -> [Attribute] -> Attribute ;
    -- ConjKind
    --   : Conjunction -> [Kind] -> Kind ;

}
