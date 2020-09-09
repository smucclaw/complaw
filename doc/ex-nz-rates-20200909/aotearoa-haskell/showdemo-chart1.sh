#!/bin/zsh

source showdemo-utils.sh; clear

vars=(combined_income=12000,13000..30000
      rates_total=100,200..2000
      dependants=0)

runcmd=(stack exec aotearoa-exe -- $params $vars --goal=l4/rates-demo.l4)

$runcmd

