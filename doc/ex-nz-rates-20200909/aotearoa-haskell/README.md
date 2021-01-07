# l4-aotearoa

In the future, L4 may import OpenFisca format code and, while
pretending to be Python, perform abstract interpretation to detect
intermediate negative numbers.

Associated presentation: [20190514 Multilateral Isomorphism in L4](https://docs.google.com/presentation/d/1U4pQFXuVAocbwzF1nPtyhxH2SBza_7nEErLPE2lnAAw/edit#)

## Installation

This is a Haskell Stack project.

https://docs.haskellstack.org/en/stable/README/

Once you have Stack installed, run `stack build`

    openfisca-aotearoa/l4% stack build
    WARNING: Ignoring out of range dependency (allow-newer enabled): containers-0.6.0.1. language-python requires: ==0.5.*
    Building all executables for `aotearoa' once. After a successful build of all of them, only specified executables will be rebuilt.
    aotearoa-0.1.0.0: configure (lib + exe)
    Configuring aotearoa-0.1.0.0...
    aotearoa-0.1.0.0: build (lib + exe)
    Preprocessing library for aotearoa-0.1.0.0..
    Building library for aotearoa-0.1.0.0..
    [1 of 2] Compiling Lib              ( src/Lib.hs, .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/Lib.o )
    [2 of 2] Compiling Paths_aotearoa   ( .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/autogen/Paths_aotearoa.hs, .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/Paths_aotearoa.o )
    Preprocessing executable 'aotearoa-exe' for aotearoa-0.1.0.0..
    Building executable 'aotearoa-exe' for aotearoa-0.1.0.0..
    [1 of 2] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/aotearoa-exe/aotearoa-exe-tmp/Main.o )
    [2 of 2] Compiling Paths_aotearoa   ( .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/aotearoa-exe/autogen/Paths_aotearoa.hs, .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/aotearoa-exe/aotearoa-exe-tmp/Paths_aotearoa.o )
    Linking .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/aotearoa-exe/aotearoa-exe ...
    aotearoa-0.1.0.0: copy/register
    Installing library in /Users/mengwong/src/l/openfisca-aotearoa/l4/.stack-work/install/x86_64-osx/lts-13.19/8.6.4/lib/x86_64-osx-ghc-8.6.4/aotearoa-0.1.0.0-GG9eVmbZRyl8VaPZpE6ywn
    Installing executable aotearoa-exe in /Users/mengwong/src/l/openfisca-aotearoa/l4/.stack-work/install/x86_64-osx/lts-13.19/8.6.4/bin
    Registering library for aotearoa-0.1.0.0..

Run the executable:

    openfisca-aotearoa/l4% stack exec aotearoa-exe -- combined_income=20000 dependants=0 rates_total=2000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=rr-br3nda-clip --nlgstyle=concrete
    showing how we obtain the answer
    630.00 -- which is
    | the lesser of
    | 1226.67 -- which is
    | | the greater of
    | | | 0.00
    | | and
    | | 1226.67 -- which is
    | | | the amount by which
    | | | 1226.67 -- which is
    | | | | the excess rates amount, which is
    | | | | two-thirds
    | | | | 1840.00 -- which is
    | | | | | the greater of
    | | | | | | 0.00
    | | | | | and
    | | | | | 1840.00 -- which is
    | | | | | | the amount by which
    | | | | | | | rates_total (which has the value 2000.00)
    | | | | | | exceeds
    | | | | | | | initial_contribution (which has the value 160.00)
    | | | exceeds
    | | | 0.00 -- which is
    | | | | the greater of
    | | | | | 0.00
    | | | | and
    | | | | -647.50 -- which is
    | | | | | the income taper amount, which is
    | | | | | the quotient given by
    | | | | | -5180.00 -- which is
    | | | | | | the income component adjusted for dependants, which is
    | | | | | | the amount by which
    | | | | | | | combined_income (which has the value 20000.00)
    | | | | | | exceeds
    | | | | | | 25180.00 -- which is
    | | | | | | | the income taper trigger, which is
    | | | | | | | the sum of
    | | | | | | | | income_threshold (which has the value 25180.00)
    | | | | | | | with
    | | | | | | | 0.00 -- which is
    | | | | | | | | the product of
    | | | | | | | | | additional_per_dependant (which has the value 500.00)
    | | | | | | | | multiplied by
    | | | | | | | | | dependants (which has the value 0.00)
    | | | | | divided by
    | | | | | | 8.00
    | and
    | | maximum_allowable (which has the value 630.00)

## NLG Styles

Try other --nlgstyle values:
- algebra
- concrete
- neutral
- abstract
- obfuscated
- yaml

Obfuscated may get you text which is eerily similar to actual legislation!

Concrete shows all numerical values used to arrive at the answer.

Abstract has the fewest numerical values.

    20190509-16:36:50 mengwong@venice4:~/src/l/openfisca-aotearoa/l4% stack exec aotearoa-exe -- dependants=0 combined_income=24000 rates_total=1000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=rr-br3nda-noclip --nlgstyle=abstract
    the goal is defined as
    | the lesser of
    | | the greater of
    | | | 0.00
    | | and
    | | | the amount by which
    | | | | the excess rates amount, which is
    | | | | two-thirds
    | | | | | the greater of
    | | | | | | 0.00
    | | | | | and
    | | | | | | the amount by which
    | | | | | | | the total rates for the property
    | | | | | | exceeds
    | | | | | | | the initial contribution by ratepayer
    | | | exceeds
    | | | | the income taper amount, which is
    | | | | the quotient given by
    | | | | | the income component adjusted for dependants, which is
    | | | | | the amount by which
    | | | | | | the ratepayer's income for the preceding tax year
    | | | | | exceeds
    | | | | | | the income taper trigger, which is
    | | | | | | the sum of
    | | | | | | | the income threshold
    | | | | | | with
    | | | | | | | the product of
    | | | | | | | | the additional allowable income per dependant
    | | | | | | | multiplied by
    | | | | | | | | the number of Persons classified as dependant for the purposes of Rates Rebates
    | | | | divided by
    | | | | | 8.00
    | and
    | | the maximum rebate allowed

Neutral is kind of a mix between the two.

Yaml produces test cases suitable for openfisca test.

## Ranges

You can also view tabular output by giving input ranges, using Haskell syntax:
https://stackoverflow.com/a/7958408:

    openfisca-aotearoa/l4% stack exec aotearoa-exe -- dependants=0 combined_income=0,2000..40000 rates_total=0,200..3000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=rr-br3nda-clip      +-----------------------------------------------------------------------------------------------------------------------------+
    |       0 2000 4000 6000 8000 10000 12000 14000 16000 18000 20000 22000 24000 26000 28000 30000 32000 34000 36000 38000 40000 |
    |   0   0    0    0    0    0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0 |
    | 200  27   27   27   27   27    27    27    27    27    27    27    27    27     0     0     0     0     0     0     0     0 |
    | 400 160  160  160  160  160   160   160   160   160   160   160   160   160    58     0     0     0     0     0     0     0 |
    | 600 293  293  293  293  293   293   293   293   293   293   293   293   293   191     0     0     0     0     0     0     0 |
    | 800 427  427  427  427  427   427   427   427   427   427   427   427   427   324    74     0     0     0     0     0     0 |
    |1000 560  560  560  560  560   560   560   560   560   560   560   560   560   458   208     0     0     0     0     0     0 |
    |1200 630  630  630  630  630   630   630   630   630   630   630   630   630   591   341    91     0     0     0     0     0 |
    |1400 630  630  630  630  630   630   630   630   630   630   630   630   630   630   474   224     0     0     0     0     0 |
    |1600 630  630  630  630  630   630   630   630   630   630   630   630   630   630   608   358   108     0     0     0     0 |
    |1800 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   491   241     0     0     0     0 |
    |2000 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   624   374   124     0     0     0 |
    |2200 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   508   258     8     0     0 |
    |2400 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   391   141     0     0 |
    |2600 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   524   274    24     0 |
    |2800 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   630   408   158     0 |
    |3000 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   630   541   291    41 |
    +-----------------------------------------------------------------------------------------------------------------------------+
    ## done with run; 336 answers across 21 variations of combined_income * 16 variations of rates_total

    openfisca-aotearoa/l4% stack exec aotearoa-exe -- dependants=0 combined_income=0,2000..40000 rates_total=0,200..3000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=rr-br3nda-noclip
    +-----------------------------------------------------------------------------------------------------------------------------+
    |       0 2000 4000 6000 8000 10000 12000 14000 16000 18000 20000 22000 24000 26000 28000 30000 32000 34000 36000 38000 40000 |
    |   0 630  630  630  630  630   630   630   630   630   630   630   398   148     0     0     0     0     0     0     0     0 |
    | 200 630  630  630  630  630   630   630   630   630   630   630   424   174     0     0     0     0     0     0     0     0 |
    | 400 630  630  630  630  630   630   630   630   630   630   630   558   308    58     0     0     0     0     0     0     0 |
    | 600 630  630  630  630  630   630   630   630   630   630   630   630   441   191     0     0     0     0     0     0     0 |
    | 800 630  630  630  630  630   630   630   630   630   630   630   630   574   324    74     0     0     0     0     0     0 |
    |1000 630  630  630  630  630   630   630   630   630   630   630   630   630   458   208     0     0     0     0     0     0 |
    |1200 630  630  630  630  630   630   630   630   630   630   630   630   630   591   341    91     0     0     0     0     0 |
    |1400 630  630  630  630  630   630   630   630   630   630   630   630   630   630   474   224     0     0     0     0     0 |
    |1600 630  630  630  630  630   630   630   630   630   630   630   630   630   630   608   358   108     0     0     0     0 |
    |1800 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   491   241     0     0     0     0 |
    |2000 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   624   374   124     0     0     0 |
    |2200 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   508   258     8     0     0 |
    |2400 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   391   141     0     0 |
    |2600 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   524   274    24     0 |
    |2800 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   630   408   158     0 |
    |3000 630  630  630  630  630   630   630   630   630   630   630   630   630   630   630   630   630   630   541   291    41 |
    +-----------------------------------------------------------------------------------------------------------------------------+
    ## done with run; 336 answers across 21 variations of combined_income * 16 variations of rates_total
    20190509-16:33:43 mengwong@venice4:~/src/l/openfisca-aotearoa/l4%

Ranging anything other than two variables takes you back to a more traditional table.

    20190509-16:34:54 mengwong@venice4:~/src/l/openfisca-aotearoa/l4% stack exec aotearoa-exe -- dependants=0,2 combined_income=0,20000..40000 rates_total=1000,2000..3000 additional_per_dependant=500 initial_contribution=160 maximum_allowable=630 income_threshold=25180 --goal=rr-br3nda-noclip
    +---------------------------------------------+
    | dependants combined_income rates_total goal |
    |          0               0        1000  630 |
    |          0               0        2000  630 |
    |          0               0        3000  630 |
    |          0           20000        1000  630 |
    |          0           20000        2000  630 |
    |          0           20000        3000  630 |
    |          0           40000        1000    0 |
    |          0           40000        2000    0 |
    |          0           40000        3000   41 |
    |          1               0        1000  630 |
    |          1               0        2000  630 |
    |          1               0        3000  630 |
    |          1           20000        1000  630 |
    |          1           20000        2000  630 |
    |          1           20000        3000  630 |
    |          1           40000        1000    0 |
    |          1           40000        2000    0 |
    |          1           40000        3000  103 |
    |          2               0        1000  630 |
    |          2               0        2000  630 |
    |          2               0        3000  630 |
    |          2           20000        1000  630 |
    |          2           20000        2000  630 |
    |          2           20000        3000  630 |
    |          2           40000        1000    0 |
    |          2           40000        2000    0 |
    |          2           40000        3000  166 |
    +---------------------------------------------+
    ## done with run; 27 answers across 3 variations of dependants * 3 variations of combined_income * 3 variations of rates_total

## Creating Your Own L4-Rules Expressions

The language available here is a fragment of L4 specialized for mathematical expressions, sufficient to run this test case. Edit `src/Lib.hs` to add new goals, using our fairly textbook s-expression syntax.

## Roadmap

Future releases of this software may integrate other L4 modules to:

- parse YAML, so it can import tests

- parse Python, (or at least OpenFisca's subset of Python), so it can read existing code

- expose a statement-oriented mathematical language allowing `let` expressions, i.e. variable assignment.

- perform abstract interpretation to seek out invariant violations e.g. no possibility of negative numbers in intermediate terms

    a - b = c

    if b > a then warn "c negative"

- expand the expressiveness of the natural language generation to more closely approximate statutory English; support other dialects as well

- output to other natural languages like Québécois

- output to LegalRuleML, SBVR, OWL

- output to other languages like XAlgorithms and Accord Project's Ergo

- output to Excel format with formulas built in

