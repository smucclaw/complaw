:- discontiguous permittedAction/3.
:- set_prolog_flag(verbose,silent).

%% explicit regulative rules.
%% it is permitted for a person to eat ribeye IF the person is an adult.
permittedAction(Person,eat,ribeye)         :- isAdult(Person).
%% if you come from a mathematical logic background, you can read ":-" as the left-implication arrow "<-".
%% if you come from a functional background, you can think of the Horn clause as meaning:
%% permittedAction is a ternary relation of type Person -> Verb -> Noun -> Maybe Boolean.
%% this three-valued logic arises from the Well-Founded Semantics: https://en.wikipedia.org/wiki/Well-founded_semantics
%% Logic Programming basically tries to please you by looking for ways to return Just True.
%% here, for the permittedAction(Person,...) function to return a Just True, isAdult(Person) has to return Just True.
%% isAdult is a unary function of type Person -> Maybe Boolean.
%% You can think of the fact that "eat" and "ribeye" appear in the function parameters, as an example of destructuring.

%% it is permitted for a person to eat mashed potatoes if the person is a baby.
permittedAction(Person,eat,mashedPotatoes) :- isBaby(Person).

%% facts about individuals
birthDate(alice, 2019).
birthDate(  bob, 1960).

%% In a normal language, you'd expect a function to return a value: calling thisYear() would return 2020.
%% But in Prolog, all Prolog expressions are ultimately typed Maybe Boolean. So we need to find a different way
%% to set return values. We know that call-by-reference allows a language to park return values inside "function parameters".
%% For the purposes of this example, we can pretend that this is what is going on.
%% The question specified that we're evaluating our rules as of the year 2020.
%% So calls elsewhere to thisYear(TY) will set TY to 2020.
thisYear(2020).

%% world knowledge: constitutive rules about babyhood and adulthood
%% calling birthDate(alice,BY) sets BY to 2019. This is called "term unification".
%% a person is an adult if, letting TY be 2020, and letting BY be the person's birthdate, their current age (TY-BY) is >= 21 years.
isAdult(Person) :- thisYear(TY), birthDate(Person,BY), TY-BY >= 21.
isBaby(Person)  :- thisYear(TY), birthDate(Person,BY), TY-BY <  4.

%% inferred regulative rules from "common-sense world knowledge":
%% - adults can do whatever they want by default; so they can eat anything, so long that thing is a member of the class of foods
permittedAction(Person,eat,O) :- isAdult(Person), isMember(O,food).
%% - but babies can't eat anything explicitly denied to them.
permittedAction(Person,eat,_) :- isBaby(Person), false.

%% "ontology" and world knowledge. we contrive an ad-hoc class model because plain Prolog doesn't have one.
isMember(mashedPotatoes,food).
isMember(ribeye,food).
%% we shall neglect to say that ailce and bob are persons.

%% queries
run(permittedAction(alice,eat,mashedPotatoes)).
run(permittedAction(alice,eat,ribeye)).
run(permittedAction(bob,eat,mashedPotatoes)).
run(permittedAction(bob,eat,ribeye)).

:- forall(run(PA), (write(PA), write("   "), (once(PA) -> write("true") ; write("false")), nl)).
