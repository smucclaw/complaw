abstract SAFE = Transactions ** {
  flags startcat = Sentence ;
  cat
    Action ;
    ActionDir ;
    ActionIndir ;
    ActionDirIndir ;
    [Action]{2} ;              -- sells stock to Acme and raises capital
    [ActionDir]{2} ;           -- sells today and issues (stock) at fixed valuation
    [ActionIndir]{2} ;         -- sells widgets and issues stock (at fixed valuation)
    [ActionDirIndir]{2} ;      -- sells and issues (stock) (at fixed valuation)

  fun
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : ActionDir ;

    -- Indirect object
    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : ActionDirIndir ;

    -- Complements
    AComplDir   : ActionDir -> Term -> Action ;
    AComplIndir : ActionIndir -> Term -> Action ;
    ASlashDir   : ActionDirIndir -> Term -> ActionIndir ; -- sell stock (at fixed valuation)
    ASlashIndir : ActionDirIndir -> Term -> ActionDir ;   -- sell (stock) at fixed valuation

    -- Adjuncts: make an Action need another argument
    PursuantTo : Action -> ActionIndir ;

    -- Negation of a whole Action: doesnt sell X  doesnt sell X and Y
    ANeg : Action -> Action ;

    -- Negation regarding the complements
    AComplNoneDir   : ActionDir -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneIndir : ActionIndir -> [Term] -> Action ; -- sells (X) neither to B nor to B

    -- Conjunctions
    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> ListActionDir -> ActionDir ;
    ConjSlashIndir : Conjunction -> ListActionIndir -> ActionIndir ;
    ConjSlashDirIndir : Conjunction -> ListActionDirIndir -> ActionDirIndir ;

  cat
    Event ;
    Temporal ;

  fun
    TPresent  : Temporal ;
    TPast     : Temporal ;
    TFuture   : Temporal ;
    Upon      : Event -> Temporal ;

    SAction : Temporal ->
      Term -> Action -> Sentence ; -- the company raises/raised/will raise capital (upon Event)

    Ev : Kind -> Event ; -- TODO improve

    ----------------
    -- Properties --
    ----------------
    Fixed,
    PreMoney,
    PostMoney,
    BonaFide,
    Voluntary : Attribute ;

    ForBenefit   -- general assignment for the benefit of the Companys creditors
      : Term -> Attribute ;

    WithPurpose   -- transaction with the purpose of raising capital
      : Action -> Attribute ;

    -----------
    -- Kinds --
    -----------
    Capital,

    DissolutionEvent,
    Termination,
    GeneralAssignment,

    LiquidityEvent,
    ChangeOfControl,
    DirectListing,
    InitialPublicOffering,

    EquityFinancing,
    Transaction,
    PreferredStock,
    Valuation : Kind ;

    KWhetherOr  -- dissolution event, whether voluntary or involuntary
      : [Attribute] -> Kind -> Kind ;

    SingleOrSeries : Kind -> Kind ;

    --------------------------
    -- Kinds with arguments --
    --------------------------
  cat
    KindTerm ;
    [KindTerm]{2} ; -- winding up and dissolution (of the Company)

  fun
    Liquidation,
    Dissolution,
    WindingUp
      : KindTerm ;

    ComplKind : KindTerm -> Term -> Kind ; -- liquidation of the company

    ConjSlashTerm -- "liquidation and dissolution of the company"
     : Conjunction -> ListKindTerm -> KindTerm ;

    -----------
    -- Terms --
    -----------
  fun
    Company : Term ;
    Creditors : Term -> Term ; -- the Companys creditors

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Determiner ->
      Kind -> Term ->
      Term ;

    RelIndir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporal ->     -- (will)
      ActionIndir -> -- sell(s) stock (under)
      Term ; -- the contract, under which the company sells stock

    RelDir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporal ->     -- (will)
      ActionDir ->   -- sign(s)
      Term ; -- the contract, which the company signswill sign


    --Series,   -- a series of transactions
    AnyOther  -- any other liquidation, dissolution or winding up
      : Determiner ;

}
