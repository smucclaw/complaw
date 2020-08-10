:- use_module(library(clpfd)).
:- use_module(library(julian)).
:- use_module(library(yall)).
rule(1, P, may, trade(Item))   :- rule(_,isPotato(Item)), rule(_, isLegalPotato(Item)).
rule(1, P, may, trade(Item))   :- rule(_,isPotato(Item)), hasExemption(P, from(directorOfAgriculture), that(P,may,trade(Item))).
rule(1, P, shant, trade(Item)) :- rule(_,isPotato(Item)), \+ rule(1, P, may, trade(Item)).
rule(2, isPotato(Item)) :- member(Item.get(species), ["Solanum tuberosum"]).
rule(3, isLegalPotato(Item)) :- rule(_,isPotato(Item)), rule(_,legalDates(Item.get(wasPlanted))).
rule(4,
     legalDates(Date)) :- rule(_, fullMoonDates(FMD)),
                          ( Date = FMD ; 
                            form_time(FMD,datetime(J0,_)), J1 #= J0+1, form_time(Date,datetime(J1,0)) ).
rule(5, fullMoonDates(2020-1-10)).
rule(5, fullMoonDates(2020-2-8)).
rule(5, fullMoonDates(2020-3-9)).
rule(5, fullMoonDates(2020-4-7)).
rule(5, fullMoonDates(2020-5-6)).
rule(5, fullMoonDates(2020-6-5)).
rule(5, fullMoonDates(2020-7-4)).
rule(5, fullMoonDates(2020-8-2)).
rule(5, fullMoonDates(2020-9-1)).
rule(5, fullMoonDates(2020-10-1)).
rule(5, fullMoonDates(2020-10-30)).
rule(5, fullMoonDates(2020-11-29)).
rule(5, fullMoonDates(2020-12-29)).
potato1(item{species:"Solanum tuberosum", wasPlanted:2020-1-10}).
potato2(item{species:"Solanum tuberosum", wasPlanted:2020-1-11}).
potato3(item{species:"Solanum tuberosum", wasPlanted:2020-1-12}).
hasExemption(nobody, from(no-one), that(nothing)).
