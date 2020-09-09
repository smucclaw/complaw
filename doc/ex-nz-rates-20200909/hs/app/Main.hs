module Main where

import Lib
import L4Rules
import Text.PrettyPrint (render, vcat, text)
import Data.List (intersperse)

main = do
  let myrules = [ exampleRule1
                , exampleRule2, exampleRule2a
                , exampleRule3
                , exampleRule4, exampleRule4b
                ]
  putStrLn $ render (vcat (intersperse (text "") $ mypretty <$> myrules))

-- need a set of ontology definition or inference statements; similar to a type system; similar to TS's interface{} declaration

exampleRule1 :: L4Rule
exampleRule1 =
  DefinitionRule "egm business"
  [] -- annotations
  (Nothing) -- aliases
  (Object TypeSelector "Company.generalMeetings.isExtraordinary?.business")
  (BlUnify
    (BlObj (Object TypeSelector ".level"))
    (BlObj (Object (TypeEnum "Specialness") "Special"))) -- insert Hindley-Milner here

exampleRule2 :: L4Rule
exampleRule2 =
  DefinitionRule "agm business"
  [] -- annotations
  (Nothing) -- aliases
  (Object TypeSelector "Company.generalMeetings.isAnnual?.business")
  (BlUnify
   (BlObj (Object TypeSelector ".level"))
   (BZunlessXthenY
    (BMatch
     (BlObj (Object TypeSelector ".matter"))
     (BlList (L4List DisjOr [ BlStr "the declaration of a dividend"
                            , BlOf (BlStr "consideration") (BlList (L4List ConjAnd [ BlStr "the financial statements"
                                                                                   , BlStr "the reports of the auditors"
                                                                                   , BlStr "the statements of the directors"
                                                                                   ]))
                            , BlStr "the election of directors in the place of retiring directors"
                            , BlOf (BlList (L4List DisjAnd [ BlStr "the appointment"
                                                           , BlStr "fixing of the remuneration" ]))
                              (BlStr "the auditors")
                            ])))
     -- the constraint solver should apply the unspoken rule that all business is either ordinary or special, to deduce the following alternative branch.
    -- (Just $ BlObj (Object (TypeEnum "Specialness") "Ordinary")) -- then
    Nothing
    (Just $ BlObj (Object (TypeEnum "Specialness") "Special")))) -- else

exampleRule2a :: L4Rule
exampleRule2a = DefinitionRule "metonym: the ontological essence of a meeting's business is its matter attribute"
  [Unspoken] -- annotations
  (Nothing) -- aliases
  (Object TypeSelector "Company.generalMeetings.business")
  (BlUnify (BlObj $ Object TypeSelector ":metonym") (BlObj $ Object TypeSelector ".matter"))
  -- this allows us to refer to a business's matter as the business itself.
  -- metonyms are resolved during canonicalization to terse form.

exampleRule3 :: L4Rule
exampleRule3 = RegulativeRule "must hold agm"
               [] -- annotations
               (Nothing) -- aliases
               (BBool True)
               (Corporation "Company")
               Must
               (Action (Object TypeString "hold an AGM") [("en", "hold an AGM")] Nothing)
               (Temporal Nothing Nothing (Repeating (Just (Interval (Just 1) Nothing Nothing))))
               NoHence -- hence
               LestBreach -- lest


-- REGULATIVE RULE "Companies Must Hold AGM" @ "CMHAGM"
-- SUBJECT TO ( [ this section, section 175A ] )
-- PARTY every Company
-- PMUST ( hold a ( general meeting called@ "annual general meeting" @ "AGM" )
--             [ ( text_en= "in addition to any other meeting" ) ] )
-- REPEATEDLY
--      AFTER ( end of each financial year )
--     WITHIN (CASE isListedPublicCo Company of
--                  true -> 4 months;
--                  _    -> 6 months) [ ( @ "period" ) ]

relationStore = [ L4RuleDefeasible 
                  [RuleGroupSpec (RuleGroup "_" ["this section", "section 175A"])]
                  [] -- notwithstanding
                  "Companies Must Hold AGM"
                ]

exampleRule4 = RegulativeRule "Companies Must Hold AGM"
               [PassiveVoice]
               (Just ["CMHAGM"])
               (BBool True)
               (Corporation "Company")
               Must
               (Action (Object (TypeArray TypeString) "hold an AGM")
                        [ ("en", "hold a general meeting called \"annual general meeting\"")
                        , ("en", "in addition to any other meeting") ]
                 (Just (BNot (BMatch
                              (BlObj (Object TypeString "CHMAGM"))
                               (BlObj (Object TypeString "unique constraint excludes any other label on this meeting")))
                       )))
               (Temporal Nothing Nothing (Repeating (Just (Interval (Just 1) Nothing Nothing))))
               NoHence
               NoLest

exampleRule4b = ConstitutiveRule "a listed public company"
                [Unspoken]  -- annotations
                Nothing     -- aliases
                Nothing     -- no § scope restriction means global
                (Object TypeSelector "Company")
                (Object TypeSelector ".isListedPublicCo?")
                (Object TypeBool "True")
                (BAnd [ BlAsBool (BlObj (Object TypeSelector ".isPublic?"))
                      , BlAsBool (BlObj (Object TypeSelector ".isListed?")) ])
