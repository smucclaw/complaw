

% legislative parameters. What numbers are in effect for 2018?
additional_per_dependant(2018,500).
income_threshold(2018,24790).
maximum_allowable(2018,620).
initial_contribution(2018,160).
rates_adjustment_ratio(2018,N) :- N is 2/3.

% user variables. What were Bob's numbers for 2018?
dependants(2018,bob,0).
combined_income(2018,bob,32103).
rates_total(2018,bob,2000).

rebate(AsOf,Person,R) :-
    maximum_allowable(AsOf,MA),
    excess_rates(AsOf,Person,ER),
    income_taper(AsOf,Person,IT),
    AsCalculated is max(0,ER - IT),
    R is min(MA,AsCalculated).

excess_rates(AsOf,Person,ER) :- rates_total(AsOf,Person,RT),
                                initial_contribution(AsOf,IC),
                                rates_adjustment_ratio(AsOf,RAR),
                                ER is (RT-IC) * RAR.

income_taper(AsOf,Person,ITaper) :- combined_income(AsOf,Person,CI),
                                    income_taper_trigger(AsOf,Person,ITT),
                                    ITaper is 1/8 * (CI-ITT).

dependant_additionals(AsOf,Person,DA) :- dependants(AsOf,Person,D),
                                         additional_per_dependant(AsOf,APD),
                                         DA is APD * D.

income_taper_trigger(AsOf,Person,ITT) :- dependant_additionals(AsOf,Person,DA),
                                         income_threshold(AsOf,IT),
                                         ITT is DA + IT.
                                         


                                         
