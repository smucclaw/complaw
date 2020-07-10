:- use_module(library(clpq)).
:- use_module(library(yall)).

  test1(Convertibles, SeriesA) :-
      Convertibles = [ Safe_A, Safe_B ],
      SeriesA = eqr{valuationPre:   15000000,
		    new_money_in:    5000000,
		    commonPre:       9250000,
                    optionsPreOutstanding: 300000,
		    optionsPrePromised:    350000,
                    optionsPreFree: 100000,
                    optionsPost:    10 / 100,
                    convertibles:   [Safe_A, Safe_B],
                    incoming:       [SeriesA_c, SeriesA_b, SeriesA_other]
		   },
      SeriesA_c     = equity{owner:"Investor C", money_in:4000000,   shareClass:"A"},
      SeriesA_b     = equity{owner:"Investor B", money_in:499998.97, shareClass:"A"},
      SeriesA_other = equity{owner:"Other New Investors", money_in:500001.19, shareClass:"A"},
      Safe_A        =   safe{owner:"Investor A", money_in:200000, val_cap:4000000},
      Safe_B        =   safe{owner:"Investor B", money_in:800000, val_cap:8000000}.

dilutionDueTo(ValPre, Safe, Dil, Log) :- effectiveValuation(ValPre, Safe, EffVal, Log),
					 Dil = Safe.money_in / EffVal.

effectiveValuation(ValPre, safe{owner:O, money_in: _                          }, EffVal, Log) :- EffVal = ValPre,
												  string_concat(O, ": no cap no discount", Log).
effectiveValuation(ValPre, safe{owner:O, money_in: _,              val_cap: PC}, EffVal, Log) :- ( ValPre <  PC -> EffVal = ValPre; EffVal = PC ),
												  string_concat(O, ": cap no discount", Log).
effectiveValuation(ValPre, safe{owner:O, money_in: _, discount: D             }, EffVal, Log) :- EffVal = (ValPre * (1-D)),
												  string_concat(O, ": discount no cap", Log).
effectiveValuation(ValPre, safe{owner:O, money_in: _, discount: D, val_cap: PC}, EffVal, Log) :- Discounted = (ValPre * (1-D)),
												  ( Discounted <  PC -> EffVal = Discounted ;  EffVal = PC),
												  string_concat(O, ": cap and discount", Log).

%% 0.15
conversionDilutions(Convertibles, ValPre, DilPercentage) :-
    maplist({ValPre}/[Convertible, Dil, Log]>>dilutionDueTo(ValPre,Convertible,Dil,Log), Convertibles, Dils, Logs),
    foldl(add,Dils,0,DilPercentage),
    forall(nth1(I, Logs, LogLine), print_message(informational, conversion_dilution(LogLine))).

conversionShares(SeriesA,CC_Roughly,Safe,NumConversionShares) :-
    dilutionDueTo(SeriesA.valuationPre, Safe, Dil, Log),
    { NumConversionShares = Dil * CC_Roughly }.

getMoneyIn(Incoming,MI) :- Incoming.money_in = MI.

conversion(SeriesA,CompanyCapitalization,ConversionSharesAll) :-
    format('* attempting conversion calculations for Series A~n', []),
    conversionDilutions(SeriesA.convertibles,SeriesA.valuationPre,DilPercentage),
    format('** dilutions are total ~w~n', [DilPercentage]),
    { SharesPre = (SeriesA.commonPre + SeriesA.optionsPreOutstanding + SeriesA.optionsPrePromised + SeriesA.optionsPreFree) },
    format('** SharesPre = ~w~n', [SharesPre]),
    { ConversionSharesAll = DilPercentage * SharesPre / (1 - DilPercentage) },
    format('** computed conversion shares = ~w~n', [ConversionSharesAll]),
    { CompanyCapitalization = SharesPre + ConversionSharesAll },
    format('** computed company capitalization = ~w~n', [CompanyCapitalization]),
    maplist(getMoneyIn, SeriesA.incoming, MIlist),
    sum_list(MIlist, AllInvestorMoney),
    format('** computed AllInvestorMoney = ~w~n', [AllInvestorMoney]),
    {
    TotalPost = CompanyCapitalization + AllInvestorIssues + OptionsNewFree,
    TotalPost > 0,
    OptionsNewFree = SeriesA.optionsPost * TotalPost - SeriesA.optionsPreFree,
    PricePerShare = SeriesA.valuationPre / ( CompanyCapitalization + OptionsNewFree ),
    AllInvestorIssues = AllInvestorMoney / PricePerShare
    },
    format('** computed pricePerShare = ~w, totalPost = ~w~n', [PricePerShare, TotalPost])
.

  :- multifile prolog:message//1.
  prolog:message(conversion_dilution(LogLine)) -->
      [ LogLine ].

  main :- test1(Convertibles, SeriesA), conversion(SeriesA,CC,NCSA).

  add(X,Y,Sum) :- Sum is X+Y.
