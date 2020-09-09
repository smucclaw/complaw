#!/bin/zsh

params=(additional_per_dependant=500
        initial_contribution=160
        maximum_allowable=630
        income_threshold=25180
        period=2019)

vars=(combined_income=24000
      rates_total=1000
      dependants=0)

runcmd=(stack exec aotearoa-exe -- $params $vars --goal=l4/rates-demo.l4)

