:- use_module(library(clpq)).
:- use_module(library(yall)).
isEquityFinancing(TxnGroup) :-
    bonafide_OK(TxnGroup, true),
    arity_OK(TxnGroup, [single, series]),
    pp_OK(TxnGroup, "raising capital"),
    pursuant_OK(TxnGroup).

%% predicates testing the transaction group as a whole

bonafide_OK(TG, X) :- TG.bonafide = X.
arity_OK(TG, [single|Xs]) :- (length(TG.txns,L), L = 1) ; arity_OK(TG, Xs).
arity_OK(TG, [series|Xs]) :- (length(TG.txns,L), L > 1) ; arity_OK(TG, Xs).
pp_OK(TG, X) :- attrMatch(TG, principalPurpose, X).

%% utils

attrMatch(Object, Attr, Val) :- Object.Attr = Val.
%% the "pursuant" thing means we have to test agreements in the aggregate
pursuant_OK(TG) :- maplist([Ag]>>agreement_OK(Ag), TG.txns).

agreement_OK(Ag) :- issues_OK(Ag,    true),
                    sells_OK(Ag,     true),
                    security_OK(Ag,  preferred),
                    valuation_OK(Ag, fixed).

%% the following predicates test individual agreements

issues_OK(   Agreement, X) :- attrMatch(Agreement, issues,   X).
sells_OK(    Agreement, X) :- attrMatch(Agreement, sells,    X).
security_OK( Agreement, X) :- attrMatch(Agreement, security, X).
valuation_OK(Agreement, X) :- attrMatch(Agreement, valuation, X).

setup1(txngroup{bonafide:true,
                principalPurpose:"raising capital",
                txns: [ agreement{ parties:  [acme, rich],
                                   issues:    true,
                                   sells:     true,
                                   security:  preferred,
                                   valuation: fixed,
                                   val_ppo:   pre_money } ] }).

setup1b(TG1b) :- setup1(TG1), TG1b = TG1.put([principalPurpose:"evading taxes"]).

setup0(txngroup{bonafide:true,
                principalPurpose:"raising capital",
                txns: [ ] }).
