{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module DMN13'xsd
  ( module DMN13'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
-- ^ Include the DMN Diagram Interchange (DI) schema
import DMNDI13'xsd as Dmndi
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementDMNElement :: XMLParser TDMNElement
elementDMNElement = fmap supertype elementContextEntry
                    `onFail`
                    fmap supertype elementArtifact
                    `onFail`
                    fmap supertype elementFunctionItem
                    `onFail`
                    fmap supertype elementAuthorityRequirement
                    `onFail`
                    fmap supertype elementKnowledgeRequirement
                    `onFail`
                    fmap supertype elementInformationRequirement
                    `onFail`
                    fmap supertype elementNamedElement
                    `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <DMNElement>,\n\
\  namely one of:\n\
\<contextEntry>, <artifact>, <functionItem>, <authorityRequirement>, <knowledgeRequirement>, <informationRequirement>, <namedElement>"
elementToXMLDMNElement :: TDMNElement -> [Content ()]
elementToXMLDMNElement = schemaTypeToXML "DMNElement"
 
data TDMNElement = TDMNElement
        { tDMNElement_id :: Maybe Xsd.ID
        , tDMNElement_label :: Maybe Xsd.XsdString
        , tDMNElement_description :: Maybe Xsd.XsdString
        , tDMNElement_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TDMNElement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TDMNElement a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TDMNElement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDMNElement_id x
                       , maybe [] (toXMLAttribute "label") $ tDMNElement_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDMNElement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDMNElement_extensionElements x
            ]
 
elementNamedElement :: XMLParser TNamedElement
elementNamedElement = fmap supertype elementInformationItem
                      `onFail`
                      fmap supertype elementItemDefinition
                      `onFail`
                      fmap supertype elementDrgElement
                      `onFail`
                      fmap supertype elementElementCollection
                      `onFail`
                      fmap supertype elementImport
                      `onFail`
                      fmap supertype elementDefinitions
                      `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <namedElement>,\n\
\  namely one of:\n\
\<informationItem>, <itemDefinition>, <drgElement>, <elementCollection>, <import>, <definitions>"
elementToXMLNamedElement :: TNamedElement -> [Content ()]
elementToXMLNamedElement = schemaTypeToXML "namedElement"
 
data TNamedElement = TNamedElement
        { tNamedElement_id :: Maybe Xsd.ID
        , tNamedElement_label :: Maybe Xsd.XsdString
        , tNamedElement_name :: Xsd.XsdString
        , tNamedElement_description :: Maybe Xsd.XsdString
        , tNamedElement_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TNamedElement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TNamedElement a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TNamedElement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tNamedElement_id x
                       , maybe [] (toXMLAttribute "label") $ tNamedElement_label x
                       , toXMLAttribute "name" $ tNamedElement_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tNamedElement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tNamedElement_extensionElements x
            ]
instance Extension TNamedElement TDMNElement where
    supertype (TNamedElement a0 a1 a2 e0 e1) =
               TDMNElement a0 a1 e0 e1
 
data TDMNElementReference = TDMNElementReference
        { tDMNElementReference_href :: Xsd.AnyURI
        }
        deriving (Eq,Show)
instance SchemaType TDMNElementReference where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "href" e pos
        commit $ interior e $ return (TDMNElementReference a0)
    schemaTypeToXML s x@TDMNElementReference{} =
        toXMLElement s [ toXMLAttribute "href" $ tDMNElementReference_href x
                       ]
            []
 
elementDefinitions :: XMLParser TDefinitions
elementDefinitions = parseSchemaType "definitions"
elementToXMLDefinitions :: TDefinitions -> [Content ()]
elementToXMLDefinitions = schemaTypeToXML "definitions"
 
data TDefinitions = TDefinitions
        { tDefinitions_id :: Maybe Xsd.ID
        , tDefinitions_label :: Maybe Xsd.XsdString
        , tDefinitions_name :: Xsd.XsdString
        , tDefinitions_expressionLanguage :: Maybe Xsd.AnyURI
        , tDefinitions_typeLanguage :: Maybe Xsd.AnyURI
        , tDefinitions_namespace :: Xsd.AnyURI
        , tDefinitions_exporter :: Maybe Xsd.XsdString
        , tDefinitions_exporterVersion :: Maybe Xsd.XsdString
        , tDefinitions_description :: Maybe Xsd.XsdString
        , tDefinitions_extensionElements :: Maybe ExtensionElements
        , tDefinitions_import :: [TImport]
        , tDefinitions_itemDefinition :: [TItemDefinition]
        , tDefinitions_drgElement :: [TDRGElement]
        , tDefinitions_artifact :: [TArtifact]
        , tDefinitions_elementCollection :: [TElementCollection]
        , tDefinitions_businessContextElement :: [TBusinessContextElement]
        , tDefinitions_Dmndi_dMNDI :: Maybe Dmndi.DMNDI
        }
        deriving (Eq,Show)
instance SchemaType TDefinitions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "expressionLanguage" e pos
        a4 <- optional $ getAttribute "typeLanguage" e pos
        a5 <- getAttribute "namespace" e pos
        a6 <- optional $ getAttribute "exporter" e pos
        a7 <- optional $ getAttribute "exporterVersion" e pos
        commit $ interior e $ return (TDefinitions a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (elementImport)
            `apply` many (parseSchemaType "itemDefinition")
            `apply` many (elementDrgElement)
            `apply` many (elementArtifact)
            `apply` many (parseSchemaType "elementCollection")
            `apply` many (elementBusinessContextElement)
            `apply` optional (elementDMNDI)
    schemaTypeToXML s x@TDefinitions{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDefinitions_id x
                       , maybe [] (toXMLAttribute "label") $ tDefinitions_label x
                       , toXMLAttribute "name" $ tDefinitions_name x
                       , maybe [] (toXMLAttribute "expressionLanguage") $ tDefinitions_expressionLanguage x
                       , maybe [] (toXMLAttribute "typeLanguage") $ tDefinitions_typeLanguage x
                       , toXMLAttribute "namespace" $ tDefinitions_namespace x
                       , maybe [] (toXMLAttribute "exporter") $ tDefinitions_exporter x
                       , maybe [] (toXMLAttribute "exporterVersion") $ tDefinitions_exporterVersion x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDefinitions_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDefinitions_extensionElements x
            , concatMap (elementToXMLImport) $ tDefinitions_import x
            , concatMap (schemaTypeToXML "itemDefinition") $ tDefinitions_itemDefinition x
            , concatMap (elementToXMLDrgElement) $ tDefinitions_drgElement x
            , concatMap (elementToXMLArtifact) $ tDefinitions_artifact x
            , concatMap (schemaTypeToXML "elementCollection") $ tDefinitions_elementCollection x
            , concatMap (elementToXMLBusinessContextElement) $ tDefinitions_businessContextElement x
            , maybe [] (elementToXMLDMNDI) $ tDefinitions_Dmndi_dMNDI x
            ]
instance Extension TDefinitions TNamedElement where
    supertype (TDefinitions a0 a1 a2 a3 a4 a5 a6 a7 e0 e1 e2 e3 e4 e5 e6 e7 e8) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TDefinitions TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDefinitions -> TNamedElement)
              
 
elementImport :: XMLParser TImport
elementImport = parseSchemaType "import"
elementToXMLImport :: TImport -> [Content ()]
elementToXMLImport = schemaTypeToXML "import"
 
data TImport = TImport
        { tImport_id :: Maybe Xsd.ID
        , tImport_label :: Maybe Xsd.XsdString
        , tImport_name :: Xsd.XsdString
        , tImport_namespace :: Xsd.AnyURI
        , tImport_locationURI :: Maybe Xsd.AnyURI
        , tImport_importType :: Xsd.AnyURI
        , tImport_description :: Maybe Xsd.XsdString
        , tImport_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TImport where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- getAttribute "namespace" e pos
        a4 <- optional $ getAttribute "locationURI" e pos
        a5 <- getAttribute "importType" e pos
        commit $ interior e $ return (TImport a0 a1 a2 a3 a4 a5)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TImport{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tImport_id x
                       , maybe [] (toXMLAttribute "label") $ tImport_label x
                       , toXMLAttribute "name" $ tImport_name x
                       , toXMLAttribute "namespace" $ tImport_namespace x
                       , maybe [] (toXMLAttribute "locationURI") $ tImport_locationURI x
                       , toXMLAttribute "importType" $ tImport_importType x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tImport_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tImport_extensionElements x
            ]
instance Extension TImport TNamedElement where
    supertype (TImport a0 a1 a2 a3 a4 a5 e0 e1) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TImport TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TImport -> TNamedElement)
              
 
elementElementCollection :: XMLParser TElementCollection
elementElementCollection = parseSchemaType "elementCollection"
elementToXMLElementCollection :: TElementCollection -> [Content ()]
elementToXMLElementCollection = schemaTypeToXML "elementCollection"
 
data TElementCollection = TElementCollection
        { tElementCollection_id :: Maybe Xsd.ID
        , tElementCollection_label :: Maybe Xsd.XsdString
        , tElementCollection_name :: Xsd.XsdString
        , tElementCollection_description :: Maybe Xsd.XsdString
        , tElementCollection_extensionElements :: Maybe ExtensionElements
        , tElementCollection_drgElement :: [TDMNElementReference]
        }
        deriving (Eq,Show)
instance SchemaType TElementCollection where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TElementCollection a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "drgElement")
    schemaTypeToXML s x@TElementCollection{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tElementCollection_id x
                       , maybe [] (toXMLAttribute "label") $ tElementCollection_label x
                       , toXMLAttribute "name" $ tElementCollection_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tElementCollection_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tElementCollection_extensionElements x
            , concatMap (schemaTypeToXML "drgElement") $ tElementCollection_drgElement x
            ]
instance Extension TElementCollection TNamedElement where
    supertype (TElementCollection a0 a1 a2 e0 e1 e2) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TElementCollection TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TElementCollection -> TNamedElement)
              
 
elementDrgElement :: XMLParser TDRGElement
elementDrgElement = fmap supertype elementKnowledgeSource
                    `onFail`
                    fmap supertype elementInputData
                    `onFail`
                    fmap supertype elementInvocable
                    `onFail`
                    fmap supertype elementDecision
                    `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <drgElement>,\n\
\  namely one of:\n\
\<knowledgeSource>, <inputData>, <invocable>, <decision>"
elementToXMLDrgElement :: TDRGElement -> [Content ()]
elementToXMLDrgElement = schemaTypeToXML "drgElement"
 
data TDRGElement = TDRGElement
        { tDRGElement_id :: Maybe Xsd.ID
        , tDRGElement_label :: Maybe Xsd.XsdString
        , tDRGElement_name :: Xsd.XsdString
        , tDRGElement_description :: Maybe Xsd.XsdString
        , tDRGElement_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TDRGElement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TDRGElement a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TDRGElement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDRGElement_id x
                       , maybe [] (toXMLAttribute "label") $ tDRGElement_label x
                       , toXMLAttribute "name" $ tDRGElement_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDRGElement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDRGElement_extensionElements x
            ]
instance Extension TDRGElement TNamedElement where
    supertype (TDRGElement a0 a1 a2 e0 e1) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TDRGElement TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              
 
elementDecision :: XMLParser TDecision
elementDecision = parseSchemaType "decision"
elementToXMLDecision :: TDecision -> [Content ()]
elementToXMLDecision = schemaTypeToXML "decision"
 
data TDecision = TDecision
        { tDecision_id :: Maybe Xsd.ID
        , tDecision_label :: Maybe Xsd.XsdString
        , tDecision_name :: Xsd.XsdString
        , tDecision_description :: Maybe Xsd.XsdString
        , tDecision_extensionElements :: Maybe ExtensionElements
        , tDecision_question :: Maybe Xsd.XsdString
        , tDecision_allowedAnswers :: Maybe Xsd.XsdString
        , tDecision_variable :: Maybe TInformationItem
        , tDecision_informationRequirement :: [TInformationRequirement]
        , tDecision_knowledgeRequirement :: [TKnowledgeRequirement]
        , tDecision_authorityRequirement :: [TAuthorityRequirement]
        , tDecision_supportedObjective :: [TDMNElementReference]
        , tDecision_impactedPerformanceIndicator :: [TDMNElementReference]
        , tDecision_decisionMaker :: [TDMNElementReference]
        , tDecision_decisionOwner :: [TDMNElementReference]
        , tDecision_usingProcess :: [TDMNElementReference]
        , tDecision_usingTask :: [TDMNElementReference]
        , tDecision_expression :: Maybe TExpression
        }
        deriving (Eq,Show)
instance SchemaType TDecision where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TDecision a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "question")
            `apply` optional (parseSchemaType "allowedAnswers")
            `apply` optional (parseSchemaType "variable")
            `apply` many (parseSchemaType "informationRequirement")
            `apply` many (parseSchemaType "knowledgeRequirement")
            `apply` many (parseSchemaType "authorityRequirement")
            `apply` many (parseSchemaType "supportedObjective")
            `apply` many (parseSchemaType "impactedPerformanceIndicator")
            `apply` many (parseSchemaType "decisionMaker")
            `apply` many (parseSchemaType "decisionOwner")
            `apply` many (parseSchemaType "usingProcess")
            `apply` many (parseSchemaType "usingTask")
            `apply` optional (elementExpression)
    schemaTypeToXML s x@TDecision{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDecision_id x
                       , maybe [] (toXMLAttribute "label") $ tDecision_label x
                       , toXMLAttribute "name" $ tDecision_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDecision_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDecision_extensionElements x
            , maybe [] (schemaTypeToXML "question") $ tDecision_question x
            , maybe [] (schemaTypeToXML "allowedAnswers") $ tDecision_allowedAnswers x
            , maybe [] (schemaTypeToXML "variable") $ tDecision_variable x
            , concatMap (schemaTypeToXML "informationRequirement") $ tDecision_informationRequirement x
            , concatMap (schemaTypeToXML "knowledgeRequirement") $ tDecision_knowledgeRequirement x
            , concatMap (schemaTypeToXML "authorityRequirement") $ tDecision_authorityRequirement x
            , concatMap (schemaTypeToXML "supportedObjective") $ tDecision_supportedObjective x
            , concatMap (schemaTypeToXML "impactedPerformanceIndicator") $ tDecision_impactedPerformanceIndicator x
            , concatMap (schemaTypeToXML "decisionMaker") $ tDecision_decisionMaker x
            , concatMap (schemaTypeToXML "decisionOwner") $ tDecision_decisionOwner x
            , concatMap (schemaTypeToXML "usingProcess") $ tDecision_usingProcess x
            , concatMap (schemaTypeToXML "usingTask") $ tDecision_usingTask x
            , maybe [] (elementToXMLExpression) $ tDecision_expression x
            ]
instance Extension TDecision TDRGElement where
    supertype (TDecision a0 a1 a2 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14) =
               TDRGElement a0 a1 a2 e0 e1
instance Extension TDecision TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TDecision -> TDRGElement)
              
instance Extension TDecision TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TDecision -> TDRGElement)
              
 
elementBusinessContextElement :: XMLParser TBusinessContextElement
elementBusinessContextElement = fmap supertype elementOrganizationUnit
                                `onFail`
                                fmap supertype elementPerformanceIndicator
                                `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <businessContextElement>,\n\
\  namely one of:\n\
\<organizationUnit>, <performanceIndicator>"
elementToXMLBusinessContextElement :: TBusinessContextElement -> [Content ()]
elementToXMLBusinessContextElement = schemaTypeToXML "businessContextElement"
 
data TBusinessContextElement = TBusinessContextElement
        { tBusinessContextElement_id :: Maybe Xsd.ID
        , tBusinessContextElement_label :: Maybe Xsd.XsdString
        , tBusinessContextElement_name :: Xsd.XsdString
        , tBusinessContextElement_uRI :: Maybe Xsd.AnyURI
        , tBusinessContextElement_description :: Maybe Xsd.XsdString
        , tBusinessContextElement_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TBusinessContextElement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "URI" e pos
        commit $ interior e $ return (TBusinessContextElement a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TBusinessContextElement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tBusinessContextElement_id x
                       , maybe [] (toXMLAttribute "label") $ tBusinessContextElement_label x
                       , toXMLAttribute "name" $ tBusinessContextElement_name x
                       , maybe [] (toXMLAttribute "URI") $ tBusinessContextElement_uRI x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tBusinessContextElement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tBusinessContextElement_extensionElements x
            ]
instance Extension TBusinessContextElement TNamedElement where
    supertype (TBusinessContextElement a0 a1 a2 a3 e0 e1) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TBusinessContextElement TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TBusinessContextElement -> TNamedElement)
              
 
elementPerformanceIndicator :: XMLParser TPerformanceIndicator
elementPerformanceIndicator = parseSchemaType "performanceIndicator"
elementToXMLPerformanceIndicator :: TPerformanceIndicator -> [Content ()]
elementToXMLPerformanceIndicator = schemaTypeToXML "performanceIndicator"
 
data TPerformanceIndicator = TPerformanceIndicator
        { tPerformanceIndicator_id :: Maybe Xsd.ID
        , tPerformanceIndicator_label :: Maybe Xsd.XsdString
        , tPerformanceIndicator_name :: Xsd.XsdString
        , tPerformanceIndicator_uRI :: Maybe Xsd.AnyURI
        , tPerformanceIndicator_description :: Maybe Xsd.XsdString
        , tPerformanceIndicator_extensionElements :: Maybe ExtensionElements
        , tPerformanceIndicator_impactingDecision :: [TDMNElementReference]
        }
        deriving (Eq,Show)
instance SchemaType TPerformanceIndicator where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "URI" e pos
        commit $ interior e $ return (TPerformanceIndicator a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "impactingDecision")
    schemaTypeToXML s x@TPerformanceIndicator{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tPerformanceIndicator_id x
                       , maybe [] (toXMLAttribute "label") $ tPerformanceIndicator_label x
                       , toXMLAttribute "name" $ tPerformanceIndicator_name x
                       , maybe [] (toXMLAttribute "URI") $ tPerformanceIndicator_uRI x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tPerformanceIndicator_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tPerformanceIndicator_extensionElements x
            , concatMap (schemaTypeToXML "impactingDecision") $ tPerformanceIndicator_impactingDecision x
            ]
instance Extension TPerformanceIndicator TBusinessContextElement where
    supertype (TPerformanceIndicator a0 a1 a2 a3 e0 e1 e2) =
               TBusinessContextElement a0 a1 a2 a3 e0 e1
instance Extension TPerformanceIndicator TNamedElement where
    supertype = (supertype :: TBusinessContextElement -> TNamedElement)
              . (supertype :: TPerformanceIndicator -> TBusinessContextElement)
              
instance Extension TPerformanceIndicator TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TBusinessContextElement -> TNamedElement)
              . (supertype :: TPerformanceIndicator -> TBusinessContextElement)
              
 
elementOrganizationUnit :: XMLParser TOrganizationUnit
elementOrganizationUnit = parseSchemaType "organizationUnit"
elementToXMLOrganizationUnit :: TOrganizationUnit -> [Content ()]
elementToXMLOrganizationUnit = schemaTypeToXML "organizationUnit"
 
data TOrganizationUnit = TOrganizationUnit
        { tOrganizationUnit_id :: Maybe Xsd.ID
        , tOrganizationUnit_label :: Maybe Xsd.XsdString
        , tOrganizationUnit_name :: Xsd.XsdString
        , tOrganizationUnit_uRI :: Maybe Xsd.AnyURI
        , tOrganizationUnit_description :: Maybe Xsd.XsdString
        , tOrganizationUnit_extensionElements :: Maybe ExtensionElements
        , tOrganizationUnit_decisionMade :: [TDMNElementReference]
        , tOrganizationUnit_decisionOwned :: [TDMNElementReference]
        }
        deriving (Eq,Show)
instance SchemaType TOrganizationUnit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "URI" e pos
        commit $ interior e $ return (TOrganizationUnit a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "decisionMade")
            `apply` many (parseSchemaType "decisionOwned")
    schemaTypeToXML s x@TOrganizationUnit{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tOrganizationUnit_id x
                       , maybe [] (toXMLAttribute "label") $ tOrganizationUnit_label x
                       , toXMLAttribute "name" $ tOrganizationUnit_name x
                       , maybe [] (toXMLAttribute "URI") $ tOrganizationUnit_uRI x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tOrganizationUnit_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tOrganizationUnit_extensionElements x
            , concatMap (schemaTypeToXML "decisionMade") $ tOrganizationUnit_decisionMade x
            , concatMap (schemaTypeToXML "decisionOwned") $ tOrganizationUnit_decisionOwned x
            ]
instance Extension TOrganizationUnit TBusinessContextElement where
    supertype (TOrganizationUnit a0 a1 a2 a3 e0 e1 e2 e3) =
               TBusinessContextElement a0 a1 a2 a3 e0 e1
instance Extension TOrganizationUnit TNamedElement where
    supertype = (supertype :: TBusinessContextElement -> TNamedElement)
              . (supertype :: TOrganizationUnit -> TBusinessContextElement)
              
instance Extension TOrganizationUnit TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TBusinessContextElement -> TNamedElement)
              . (supertype :: TOrganizationUnit -> TBusinessContextElement)
              
 
elementInvocable :: XMLParser TInvocable
elementInvocable = fmap supertype elementDecisionService
                   `onFail`
                   fmap supertype elementBusinessKnowledgeModel
                   `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <invocable>,\n\
\  namely one of:\n\
\<decisionService>, <businessKnowledgeModel>"
elementToXMLInvocable :: TInvocable -> [Content ()]
elementToXMLInvocable = schemaTypeToXML "invocable"
 
data TInvocable = TInvocable
        { tInvocable_id :: Maybe Xsd.ID
        , tInvocable_label :: Maybe Xsd.XsdString
        , tInvocable_name :: Xsd.XsdString
        , tInvocable_description :: Maybe Xsd.XsdString
        , tInvocable_extensionElements :: Maybe ExtensionElements
        , tInvocable_variable :: Maybe TInformationItem
        }
        deriving (Eq,Show)
instance SchemaType TInvocable where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TInvocable a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "variable")
    schemaTypeToXML s x@TInvocable{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInvocable_id x
                       , maybe [] (toXMLAttribute "label") $ tInvocable_label x
                       , toXMLAttribute "name" $ tInvocable_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInvocable_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInvocable_extensionElements x
            , maybe [] (schemaTypeToXML "variable") $ tInvocable_variable x
            ]
instance Extension TInvocable TDRGElement where
    supertype (TInvocable a0 a1 a2 e0 e1 e2) =
               TDRGElement a0 a1 a2 e0 e1
instance Extension TInvocable TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              
instance Extension TInvocable TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              
 
elementBusinessKnowledgeModel :: XMLParser TBusinessKnowledgeModel
elementBusinessKnowledgeModel = parseSchemaType "businessKnowledgeModel"
elementToXMLBusinessKnowledgeModel :: TBusinessKnowledgeModel -> [Content ()]
elementToXMLBusinessKnowledgeModel = schemaTypeToXML "businessKnowledgeModel"
 
data TBusinessKnowledgeModel = TBusinessKnowledgeModel
        { tBusinessKnowledgeModel_id :: Maybe Xsd.ID
        , tBusinessKnowledgeModel_label :: Maybe Xsd.XsdString
        , tBusinessKnowledgeModel_name :: Xsd.XsdString
        , tBusinessKnowledgeModel_description :: Maybe Xsd.XsdString
        , tBusinessKnowledgeModel_extensionElements :: Maybe ExtensionElements
        , tBusinessKnowledgeModel_variable :: Maybe TInformationItem
        , tBusinessKnowledgeModel_encapsulatedLogic :: Maybe TFunctionDefinition
        , tBusinessKnowledgeModel_knowledgeRequirement :: [TKnowledgeRequirement]
        , tBusinessKnowledgeModel_authorityRequirement :: [TAuthorityRequirement]
        }
        deriving (Eq,Show)
instance SchemaType TBusinessKnowledgeModel where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TBusinessKnowledgeModel a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "variable")
            `apply` optional (parseSchemaType "encapsulatedLogic")
            `apply` many (parseSchemaType "knowledgeRequirement")
            `apply` many (parseSchemaType "authorityRequirement")
    schemaTypeToXML s x@TBusinessKnowledgeModel{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tBusinessKnowledgeModel_id x
                       , maybe [] (toXMLAttribute "label") $ tBusinessKnowledgeModel_label x
                       , toXMLAttribute "name" $ tBusinessKnowledgeModel_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tBusinessKnowledgeModel_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tBusinessKnowledgeModel_extensionElements x
            , maybe [] (schemaTypeToXML "variable") $ tBusinessKnowledgeModel_variable x
            , maybe [] (schemaTypeToXML "encapsulatedLogic") $ tBusinessKnowledgeModel_encapsulatedLogic x
            , concatMap (schemaTypeToXML "knowledgeRequirement") $ tBusinessKnowledgeModel_knowledgeRequirement x
            , concatMap (schemaTypeToXML "authorityRequirement") $ tBusinessKnowledgeModel_authorityRequirement x
            ]
instance Extension TBusinessKnowledgeModel TInvocable where
    supertype (TBusinessKnowledgeModel a0 a1 a2 e0 e1 e2 e3 e4 e5) =
               TInvocable a0 a1 a2 e0 e1 e2
instance Extension TBusinessKnowledgeModel TDRGElement where
    supertype = (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TBusinessKnowledgeModel -> TInvocable)
              
instance Extension TBusinessKnowledgeModel TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TBusinessKnowledgeModel -> TInvocable)
              
instance Extension TBusinessKnowledgeModel TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TBusinessKnowledgeModel -> TInvocable)
              
 
elementInputData :: XMLParser TInputData
elementInputData = parseSchemaType "inputData"
elementToXMLInputData :: TInputData -> [Content ()]
elementToXMLInputData = schemaTypeToXML "inputData"
 
data TInputData = TInputData
        { tInputData_id :: Maybe Xsd.ID
        , tInputData_label :: Maybe Xsd.XsdString
        , tInputData_name :: Xsd.XsdString
        , tInputData_description :: Maybe Xsd.XsdString
        , tInputData_extensionElements :: Maybe ExtensionElements
        , tInputData_variable :: Maybe TInformationItem
        }
        deriving (Eq,Show)
instance SchemaType TInputData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TInputData a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "variable")
    schemaTypeToXML s x@TInputData{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInputData_id x
                       , maybe [] (toXMLAttribute "label") $ tInputData_label x
                       , toXMLAttribute "name" $ tInputData_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInputData_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInputData_extensionElements x
            , maybe [] (schemaTypeToXML "variable") $ tInputData_variable x
            ]
instance Extension TInputData TDRGElement where
    supertype (TInputData a0 a1 a2 e0 e1 e2) =
               TDRGElement a0 a1 a2 e0 e1
instance Extension TInputData TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInputData -> TDRGElement)
              
instance Extension TInputData TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInputData -> TDRGElement)
              
 
elementKnowledgeSource :: XMLParser TKnowledgeSource
elementKnowledgeSource = parseSchemaType "knowledgeSource"
elementToXMLKnowledgeSource :: TKnowledgeSource -> [Content ()]
elementToXMLKnowledgeSource = schemaTypeToXML "knowledgeSource"
 
data TKnowledgeSource = TKnowledgeSource
        { tKnowledgeSource_id :: Maybe Xsd.ID
        , tKnowledgeSource_label :: Maybe Xsd.XsdString
        , tKnowledgeSource_name :: Xsd.XsdString
        , tKnowledgeSource_locationURI :: Maybe Xsd.AnyURI
        , tKnowledgeSource_description :: Maybe Xsd.XsdString
        , tKnowledgeSource_extensionElements :: Maybe ExtensionElements
        , tKnowledgeSource_authorityRequirement :: [TAuthorityRequirement]
        , tKnowledgeSource_type :: Maybe Xsd.XsdString
        , tKnowledgeSource_owner :: Maybe TDMNElementReference
        }
        deriving (Eq,Show)
instance SchemaType TKnowledgeSource where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "locationURI" e pos
        commit $ interior e $ return (TKnowledgeSource a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "authorityRequirement")
            `apply` optional (parseSchemaType "type")
            `apply` optional (parseSchemaType "owner")
    schemaTypeToXML s x@TKnowledgeSource{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tKnowledgeSource_id x
                       , maybe [] (toXMLAttribute "label") $ tKnowledgeSource_label x
                       , toXMLAttribute "name" $ tKnowledgeSource_name x
                       , maybe [] (toXMLAttribute "locationURI") $ tKnowledgeSource_locationURI x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tKnowledgeSource_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tKnowledgeSource_extensionElements x
            , concatMap (schemaTypeToXML "authorityRequirement") $ tKnowledgeSource_authorityRequirement x
            , maybe [] (schemaTypeToXML "type") $ tKnowledgeSource_type x
            , maybe [] (schemaTypeToXML "owner") $ tKnowledgeSource_owner x
            ]
instance Extension TKnowledgeSource TDRGElement where
    supertype (TKnowledgeSource a0 a1 a2 a3 e0 e1 e2 e3 e4) =
               TDRGElement a0 a1 a2 e0 e1
instance Extension TKnowledgeSource TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TKnowledgeSource -> TDRGElement)
              
instance Extension TKnowledgeSource TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TKnowledgeSource -> TDRGElement)
              
 
elementInformationRequirement :: XMLParser TInformationRequirement
elementInformationRequirement = parseSchemaType "informationRequirement"
elementToXMLInformationRequirement :: TInformationRequirement -> [Content ()]
elementToXMLInformationRequirement = schemaTypeToXML "informationRequirement"
 
data TInformationRequirement = TInformationRequirement
        { tInformationRequirement_id :: Maybe Xsd.ID
        , tInformationRequirement_label :: Maybe Xsd.XsdString
        , tInformationRequirement_description :: Maybe Xsd.XsdString
        , tInformationRequirement_extensionElements :: Maybe ExtensionElements
        , tInformationRequirement_choice2 :: OneOf2 TDMNElementReference TDMNElementReference
          -- ^ Choice between:
          --   
          --   (1) requiredDecision
          --   
          --   (2) requiredInput
        }
        deriving (Eq,Show)
instance SchemaType TInformationRequirement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TInformationRequirement a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` oneOf' [ ("TDMNElementReference", fmap OneOf2 (parseSchemaType "requiredDecision"))
                           , ("TDMNElementReference", fmap TwoOf2 (parseSchemaType "requiredInput"))
                           ]
    schemaTypeToXML s x@TInformationRequirement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInformationRequirement_id x
                       , maybe [] (toXMLAttribute "label") $ tInformationRequirement_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInformationRequirement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInformationRequirement_extensionElements x
            , foldOneOf2  (schemaTypeToXML "requiredDecision")
                          (schemaTypeToXML "requiredInput")
                          $ tInformationRequirement_choice2 x
            ]
instance Extension TInformationRequirement TDMNElement where
    supertype (TInformationRequirement a0 a1 e0 e1 e2) =
               TDMNElement a0 a1 e0 e1
 
elementKnowledgeRequirement :: XMLParser TKnowledgeRequirement
elementKnowledgeRequirement = parseSchemaType "knowledgeRequirement"
elementToXMLKnowledgeRequirement :: TKnowledgeRequirement -> [Content ()]
elementToXMLKnowledgeRequirement = schemaTypeToXML "knowledgeRequirement"
 
data TKnowledgeRequirement = TKnowledgeRequirement
        { tKnowledgeRequirement_id :: Maybe Xsd.ID
        , tKnowledgeRequirement_label :: Maybe Xsd.XsdString
        , tKnowledgeRequirement_description :: Maybe Xsd.XsdString
        , tKnowledgeRequirement_extensionElements :: Maybe ExtensionElements
        , tKnowledgeRequirement_requiredKnowledge :: TDMNElementReference
        }
        deriving (Eq,Show)
instance SchemaType TKnowledgeRequirement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TKnowledgeRequirement a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` parseSchemaType "requiredKnowledge"
    schemaTypeToXML s x@TKnowledgeRequirement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tKnowledgeRequirement_id x
                       , maybe [] (toXMLAttribute "label") $ tKnowledgeRequirement_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tKnowledgeRequirement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tKnowledgeRequirement_extensionElements x
            , schemaTypeToXML "requiredKnowledge" $ tKnowledgeRequirement_requiredKnowledge x
            ]
instance Extension TKnowledgeRequirement TDMNElement where
    supertype (TKnowledgeRequirement a0 a1 e0 e1 e2) =
               TDMNElement a0 a1 e0 e1
 
elementAuthorityRequirement :: XMLParser TAuthorityRequirement
elementAuthorityRequirement = parseSchemaType "authorityRequirement"
elementToXMLAuthorityRequirement :: TAuthorityRequirement -> [Content ()]
elementToXMLAuthorityRequirement = schemaTypeToXML "authorityRequirement"
 
data TAuthorityRequirement = TAuthorityRequirement
        { tAuthorityRequirement_id :: Maybe Xsd.ID
        , tAuthorityRequirement_label :: Maybe Xsd.XsdString
        , tAuthorityRequirement_description :: Maybe Xsd.XsdString
        , tAuthorityRequirement_extensionElements :: Maybe ExtensionElements
        , tAuthorityRequirement_choice2 :: OneOf3 TDMNElementReference TDMNElementReference TDMNElementReference
          -- ^ Choice between:
          --   
          --   (1) requiredDecision
          --   
          --   (2) requiredInput
          --   
          --   (3) requiredAuthority
        }
        deriving (Eq,Show)
instance SchemaType TAuthorityRequirement where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TAuthorityRequirement a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` oneOf' [ ("TDMNElementReference", fmap OneOf3 (parseSchemaType "requiredDecision"))
                           , ("TDMNElementReference", fmap TwoOf3 (parseSchemaType "requiredInput"))
                           , ("TDMNElementReference", fmap ThreeOf3 (parseSchemaType "requiredAuthority"))
                           ]
    schemaTypeToXML s x@TAuthorityRequirement{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tAuthorityRequirement_id x
                       , maybe [] (toXMLAttribute "label") $ tAuthorityRequirement_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tAuthorityRequirement_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tAuthorityRequirement_extensionElements x
            , foldOneOf3  (schemaTypeToXML "requiredDecision")
                          (schemaTypeToXML "requiredInput")
                          (schemaTypeToXML "requiredAuthority")
                          $ tAuthorityRequirement_choice2 x
            ]
instance Extension TAuthorityRequirement TDMNElement where
    supertype (TAuthorityRequirement a0 a1 e0 e1 e2) =
               TDMNElement a0 a1 e0 e1
 
elementExpression :: XMLParser TExpression
elementExpression = fmap supertype elementList
                    `onFail`
                    fmap supertype elementRelation
                    `onFail`
                    fmap supertype elementFunctionDefinition
                    `onFail`
                    fmap supertype elementContext
                    `onFail`
                    fmap supertype elementDecisionTable
                    `onFail`
                    fmap supertype elementInvocation
                    `onFail`
                    fmap supertype elementLiteralExpression
                    `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <expression>,\n\
\  namely one of:\n\
\<list>, <relation>, <functionDefinition>, <context>, <decisionTable>, <invocation>, <literalExpression>"
elementToXMLExpression :: TExpression -> [Content ()]
elementToXMLExpression = schemaTypeToXML "expression"
 
data TExpression = TExpression
        { tExpression_id :: Maybe Xsd.ID
        , tExpression_label :: Maybe Xsd.XsdString
        , tExpression_typeRef :: Maybe Xsd.XsdString
        , tExpression_description :: Maybe Xsd.XsdString
        , tExpression_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TExpression where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TExpression a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TExpression{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tExpression_id x
                       , maybe [] (toXMLAttribute "label") $ tExpression_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tExpression_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tExpression_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tExpression_extensionElements x
            ]
instance Extension TExpression TDMNElement where
    supertype (TExpression a0 a1 a2 e0 e1) =
               TDMNElement a0 a1 e0 e1
 
elementItemDefinition :: XMLParser TItemDefinition
elementItemDefinition = parseSchemaType "itemDefinition"
elementToXMLItemDefinition :: TItemDefinition -> [Content ()]
elementToXMLItemDefinition = schemaTypeToXML "itemDefinition"
 
data TItemDefinition = TItemDefinition
        { tItemDefinition_id :: Maybe Xsd.ID
        , tItemDefinition_label :: Maybe Xsd.XsdString
        , tItemDefinition_name :: Xsd.XsdString
        , tItemDefinition_typeLanguage :: Maybe Xsd.AnyURI
        , tItemDefinition_isCollection :: Maybe Xsd.Boolean
        , tItemDefinition_description :: Maybe Xsd.XsdString
        , tItemDefinition_extensionElements :: Maybe ExtensionElements
        , tItemDefinition_choice2 :: OneOf3 (Xsd.XsdString,(Maybe (TUnaryTests))) [TItemDefinition] (Maybe (TFunctionItem))
          -- ^ Choice between:
          --   
          --   (1) Sequence of:
          --   
          --     * typeRef
          --   
          --     * allowedValues
          --   
          --   (2) itemComponent
          --   
          --   (3) functionItem
        }
        deriving (Eq,Show)
instance SchemaType TItemDefinition where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "typeLanguage" e pos
        a4 <- optional $ getAttribute "isCollection" e pos
        commit $ interior e $ return (TItemDefinition a0 a1 a2 a3 a4)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` oneOf' [ ("Xsd.XsdString Maybe TUnaryTests", fmap OneOf3 (return (,) `apply` parseSchemaType "typeRef"
                                                                                         `apply` optional (parseSchemaType "allowedValues")))
                           , ("[TItemDefinition]", fmap TwoOf3 (many1 (parseSchemaType "itemComponent")))
                           , ("Maybe TFunctionItem", fmap ThreeOf3 (optional (parseSchemaType "functionItem")))
                           ]
    schemaTypeToXML s x@TItemDefinition{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tItemDefinition_id x
                       , maybe [] (toXMLAttribute "label") $ tItemDefinition_label x
                       , toXMLAttribute "name" $ tItemDefinition_name x
                       , maybe [] (toXMLAttribute "typeLanguage") $ tItemDefinition_typeLanguage x
                       , maybe [] (toXMLAttribute "isCollection") $ tItemDefinition_isCollection x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tItemDefinition_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tItemDefinition_extensionElements x
            , foldOneOf3  (\ (a,b) -> concat [ schemaTypeToXML "typeRef" a
                                             , maybe [] (schemaTypeToXML "allowedValues") b
                                             ])
                          (concatMap (schemaTypeToXML "itemComponent"))
                          (maybe [] (schemaTypeToXML "functionItem"))
                          $ tItemDefinition_choice2 x
            ]
instance Extension TItemDefinition TNamedElement where
    supertype (TItemDefinition a0 a1 a2 a3 a4 e0 e1 e2) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TItemDefinition TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TItemDefinition -> TNamedElement)
              
 
elementFunctionItem :: XMLParser TFunctionItem
elementFunctionItem = parseSchemaType "functionItem"
elementToXMLFunctionItem :: TFunctionItem -> [Content ()]
elementToXMLFunctionItem = schemaTypeToXML "functionItem"
 
data TFunctionItem = TFunctionItem
        { tFunctionItem_id :: Maybe Xsd.ID
        , tFunctionItem_label :: Maybe Xsd.XsdString
        , tFunctionItem_outputTypeRef :: Maybe Xsd.XsdString
        , tFunctionItem_description :: Maybe Xsd.XsdString
        , tFunctionItem_extensionElements :: Maybe ExtensionElements
        , tFunctionItem_parameters :: [TInformationItem]
        }
        deriving (Eq,Show)
instance SchemaType TFunctionItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "outputTypeRef" e pos
        commit $ interior e $ return (TFunctionItem a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "parameters")
    schemaTypeToXML s x@TFunctionItem{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tFunctionItem_id x
                       , maybe [] (toXMLAttribute "label") $ tFunctionItem_label x
                       , maybe [] (toXMLAttribute "outputTypeRef") $ tFunctionItem_outputTypeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tFunctionItem_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tFunctionItem_extensionElements x
            , concatMap (schemaTypeToXML "parameters") $ tFunctionItem_parameters x
            ]
instance Extension TFunctionItem TDMNElement where
    supertype (TFunctionItem a0 a1 a2 e0 e1 e2) =
               TDMNElement a0 a1 e0 e1
 
elementLiteralExpression :: XMLParser TLiteralExpression
elementLiteralExpression = parseSchemaType "literalExpression"
elementToXMLLiteralExpression :: TLiteralExpression -> [Content ()]
elementToXMLLiteralExpression = schemaTypeToXML "literalExpression"
 
data TLiteralExpression = TLiteralExpression
        { tLiteralExpression_id :: Maybe Xsd.ID
        , tLiteralExpression_label :: Maybe Xsd.XsdString
        , tLiteralExpression_typeRef :: Maybe Xsd.XsdString
        , tLiteralExpression_expressionLanguage :: Maybe Xsd.AnyURI
        , tLiteralExpression_description :: Maybe Xsd.XsdString
        , tLiteralExpression_extensionElements :: Maybe ExtensionElements
        , tLiteralExpression_choice2 :: (Maybe (OneOf2 Xsd.XsdString TImportedValues))
          -- ^ Choice between:
          --   
          --   (1) text
          --   
          --   (2) importedValues
        }
        deriving (Eq,Show)
instance SchemaType TLiteralExpression where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        a3 <- optional $ getAttribute "expressionLanguage" e pos
        commit $ interior e $ return (TLiteralExpression a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (oneOf' [ ("Xsd.XsdString", fmap OneOf2 (parseSchemaType "text"))
                                     , ("TImportedValues", fmap TwoOf2 (parseSchemaType "importedValues"))
                                     ])
    schemaTypeToXML s x@TLiteralExpression{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tLiteralExpression_id x
                       , maybe [] (toXMLAttribute "label") $ tLiteralExpression_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tLiteralExpression_typeRef x
                       , maybe [] (toXMLAttribute "expressionLanguage") $ tLiteralExpression_expressionLanguage x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tLiteralExpression_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tLiteralExpression_extensionElements x
            , maybe [] (foldOneOf2  (schemaTypeToXML "text")
                                    (schemaTypeToXML "importedValues")
                                   ) $ tLiteralExpression_choice2 x
            ]
instance Extension TLiteralExpression TExpression where
    supertype (TLiteralExpression a0 a1 a2 a3 e0 e1 e2) =
               TExpression a0 a1 a2 e0 e1
instance Extension TLiteralExpression TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TLiteralExpression -> TExpression)
              
 
elementInvocation :: XMLParser TInvocation
elementInvocation = parseSchemaType "invocation"
elementToXMLInvocation :: TInvocation -> [Content ()]
elementToXMLInvocation = schemaTypeToXML "invocation"
 
data TInvocation = TInvocation
        { tInvocation_id :: Maybe Xsd.ID
        , tInvocation_label :: Maybe Xsd.XsdString
        , tInvocation_typeRef :: Maybe Xsd.XsdString
        , tInvocation_description :: Maybe Xsd.XsdString
        , tInvocation_extensionElements :: Maybe ExtensionElements
        , tInvocation_expression :: Maybe TExpression
        , tInvocation_binding :: [TBinding]
        }
        deriving (Eq,Show)
instance SchemaType TInvocation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TInvocation a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (elementExpression)
            `apply` many (parseSchemaType "binding")
    schemaTypeToXML s x@TInvocation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInvocation_id x
                       , maybe [] (toXMLAttribute "label") $ tInvocation_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tInvocation_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInvocation_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInvocation_extensionElements x
            , maybe [] (elementToXMLExpression) $ tInvocation_expression x
            , concatMap (schemaTypeToXML "binding") $ tInvocation_binding x
            ]
instance Extension TInvocation TExpression where
    supertype (TInvocation a0 a1 a2 e0 e1 e2 e3) =
               TExpression a0 a1 a2 e0 e1
instance Extension TInvocation TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TInvocation -> TExpression)
              
 
data TBinding = TBinding
        { tBinding_parameter :: TInformationItem
        , tBinding_expression :: Maybe TExpression
        }
        deriving (Eq,Show)
instance SchemaType TBinding where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TBinding
            `apply` parseSchemaType "parameter"
            `apply` optional (elementExpression)
    schemaTypeToXML s x@TBinding{} =
        toXMLElement s []
            [ schemaTypeToXML "parameter" $ tBinding_parameter x
            , maybe [] (elementToXMLExpression) $ tBinding_expression x
            ]
 
elementInformationItem :: XMLParser TInformationItem
elementInformationItem = parseSchemaType "informationItem"
elementToXMLInformationItem :: TInformationItem -> [Content ()]
elementToXMLInformationItem = schemaTypeToXML "informationItem"
 
data TInformationItem = TInformationItem
        { tInformationItem_id :: Maybe Xsd.ID
        , tInformationItem_label :: Maybe Xsd.XsdString
        , tInformationItem_name :: Xsd.XsdString
        , tInformationItem_typeRef :: Maybe Xsd.XsdString
        , tInformationItem_description :: Maybe Xsd.XsdString
        , tInformationItem_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TInformationItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TInformationItem a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TInformationItem{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInformationItem_id x
                       , maybe [] (toXMLAttribute "label") $ tInformationItem_label x
                       , toXMLAttribute "name" $ tInformationItem_name x
                       , maybe [] (toXMLAttribute "typeRef") $ tInformationItem_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInformationItem_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInformationItem_extensionElements x
            ]
instance Extension TInformationItem TNamedElement where
    supertype (TInformationItem a0 a1 a2 a3 e0 e1) =
               TNamedElement a0 a1 a2 e0 e1
instance Extension TInformationItem TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TInformationItem -> TNamedElement)
              
 
elementDecisionTable :: XMLParser TDecisionTable
elementDecisionTable = parseSchemaType "decisionTable"
elementToXMLDecisionTable :: TDecisionTable -> [Content ()]
elementToXMLDecisionTable = schemaTypeToXML "decisionTable"
 
data TDecisionTable = TDecisionTable
        { tDecisionTable_id :: Maybe Xsd.ID
        , tDecisionTable_label :: Maybe Xsd.XsdString
        , tDecisionTable_typeRef :: Maybe Xsd.XsdString
        , tDecisionTable_hitPolicy :: Maybe THitPolicy
        , tDecisionTable_aggregation :: Maybe TBuiltinAggregator
        , tDecisionTable_preferredOrientation :: Maybe TDecisionTableOrientation
        , tDecisionTable_outputLabel :: Maybe Xsd.XsdString
        , tDecisionTable_description :: Maybe Xsd.XsdString
        , tDecisionTable_extensionElements :: Maybe ExtensionElements
        , tDecisionTable_input :: [TInputClause]
        , tDecisionTable_output :: [TOutputClause]
        , tDecisionTable_annotation :: [TRuleAnnotationClause]
        , tDecisionTable_rule :: [TDecisionRule]
        }
        deriving (Eq,Show)
instance SchemaType TDecisionTable where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        a3 <- optional $ getAttribute "hitPolicy" e pos
        a4 <- optional $ getAttribute "aggregation" e pos
        a5 <- optional $ getAttribute "preferredOrientation" e pos
        a6 <- optional $ getAttribute "outputLabel" e pos
        commit $ interior e $ return (TDecisionTable a0 a1 a2 a3 a4 a5 a6)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "input")
            `apply` many1 (parseSchemaType "output")
            `apply` many (parseSchemaType "annotation")
            `apply` many (parseSchemaType "rule")
    schemaTypeToXML s x@TDecisionTable{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDecisionTable_id x
                       , maybe [] (toXMLAttribute "label") $ tDecisionTable_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tDecisionTable_typeRef x
                       , maybe [] (toXMLAttribute "hitPolicy") $ tDecisionTable_hitPolicy x
                       , maybe [] (toXMLAttribute "aggregation") $ tDecisionTable_aggregation x
                       , maybe [] (toXMLAttribute "preferredOrientation") $ tDecisionTable_preferredOrientation x
                       , maybe [] (toXMLAttribute "outputLabel") $ tDecisionTable_outputLabel x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDecisionTable_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDecisionTable_extensionElements x
            , concatMap (schemaTypeToXML "input") $ tDecisionTable_input x
            , concatMap (schemaTypeToXML "output") $ tDecisionTable_output x
            , concatMap (schemaTypeToXML "annotation") $ tDecisionTable_annotation x
            , concatMap (schemaTypeToXML "rule") $ tDecisionTable_rule x
            ]
instance Extension TDecisionTable TExpression where
    supertype (TDecisionTable a0 a1 a2 a3 a4 a5 a6 e0 e1 e2 e3 e4 e5) =
               TExpression a0 a1 a2 e0 e1
instance Extension TDecisionTable TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TDecisionTable -> TExpression)
              
 
data TInputClause = TInputClause
        { tInputClause_id :: Maybe Xsd.ID
        , tInputClause_label :: Maybe Xsd.XsdString
        , tInputClause_description :: Maybe Xsd.XsdString
        , tInputClause_extensionElements :: Maybe ExtensionElements
        , tInputClause_inputExpression :: TLiteralExpression
        , tInputClause_inputValues :: Maybe TUnaryTests
        }
        deriving (Eq,Show)
instance SchemaType TInputClause where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TInputClause a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` parseSchemaType "inputExpression"
            `apply` optional (parseSchemaType "inputValues")
    schemaTypeToXML s x@TInputClause{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tInputClause_id x
                       , maybe [] (toXMLAttribute "label") $ tInputClause_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tInputClause_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tInputClause_extensionElements x
            , schemaTypeToXML "inputExpression" $ tInputClause_inputExpression x
            , maybe [] (schemaTypeToXML "inputValues") $ tInputClause_inputValues x
            ]
instance Extension TInputClause TDMNElement where
    supertype (TInputClause a0 a1 e0 e1 e2 e3) =
               TDMNElement a0 a1 e0 e1
 
data TOutputClause = TOutputClause
        { tOutputClause_id :: Maybe Xsd.ID
        , tOutputClause_label :: Maybe Xsd.XsdString
        , tOutputClause_name :: Maybe Xsd.XsdString
        , tOutputClause_typeRef :: Maybe Xsd.XsdString
        , tOutputClause_description :: Maybe Xsd.XsdString
        , tOutputClause_extensionElements :: Maybe ExtensionElements
        , tOutputClause_outputValues :: Maybe TUnaryTests
        , tOutputClause_defaultOutputEntry :: Maybe TLiteralExpression
        }
        deriving (Eq,Show)
instance SchemaType TOutputClause where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "name" e pos
        a3 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TOutputClause a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "outputValues")
            `apply` optional (parseSchemaType "defaultOutputEntry")
    schemaTypeToXML s x@TOutputClause{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tOutputClause_id x
                       , maybe [] (toXMLAttribute "label") $ tOutputClause_label x
                       , maybe [] (toXMLAttribute "name") $ tOutputClause_name x
                       , maybe [] (toXMLAttribute "typeRef") $ tOutputClause_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tOutputClause_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tOutputClause_extensionElements x
            , maybe [] (schemaTypeToXML "outputValues") $ tOutputClause_outputValues x
            , maybe [] (schemaTypeToXML "defaultOutputEntry") $ tOutputClause_defaultOutputEntry x
            ]
instance Extension TOutputClause TDMNElement where
    supertype (TOutputClause a0 a1 a2 a3 e0 e1 e2 e3) =
               TDMNElement a0 a1 e0 e1
 
data TRuleAnnotationClause = TRuleAnnotationClause
        { tRuleAnnotationClause_name :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType TRuleAnnotationClause where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        commit $ interior e $ return (TRuleAnnotationClause a0)
    schemaTypeToXML s x@TRuleAnnotationClause{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ tRuleAnnotationClause_name x
                       ]
            []
 
data TDecisionRule = TDecisionRule
        { tDecisionRule_id :: Maybe Xsd.ID
        , tDecisionRule_label :: Maybe Xsd.XsdString
        , tDecisionRule_description :: Maybe Xsd.XsdString
        , tDecisionRule_extensionElements :: Maybe ExtensionElements
        , tDecisionRule_inputEntry :: [TUnaryTests]
        , tDecisionRule_outputEntry :: [TLiteralExpression]
        , tDecisionRule_annotationEntry :: [TRuleAnnotation]
        }
        deriving (Eq,Show)
instance SchemaType TDecisionRule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TDecisionRule a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "inputEntry")
            `apply` many1 (parseSchemaType "outputEntry")
            `apply` many (parseSchemaType "annotationEntry")
    schemaTypeToXML s x@TDecisionRule{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDecisionRule_id x
                       , maybe [] (toXMLAttribute "label") $ tDecisionRule_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDecisionRule_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDecisionRule_extensionElements x
            , concatMap (schemaTypeToXML "inputEntry") $ tDecisionRule_inputEntry x
            , concatMap (schemaTypeToXML "outputEntry") $ tDecisionRule_outputEntry x
            , concatMap (schemaTypeToXML "annotationEntry") $ tDecisionRule_annotationEntry x
            ]
instance Extension TDecisionRule TDMNElement where
    supertype (TDecisionRule a0 a1 e0 e1 e2 e3 e4) =
               TDMNElement a0 a1 e0 e1
 
data TRuleAnnotation = TRuleAnnotation
        { tRuleAnnotation_text :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType TRuleAnnotation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TRuleAnnotation
            `apply` optional (parseSchemaType "text")
    schemaTypeToXML s x@TRuleAnnotation{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "text") $ tRuleAnnotation_text x
            ]
 
data THitPolicy
    = THitPolicy_UNIQUE
    | THitPolicy_FIRST
    | THitPolicy_PRIORITY
    | THitPolicy_ANY
    | THitPolicy_COLLECT
    | THitPolicy_RULE_ORDER
    | THitPolicy_OUTPUT_ORDER
    deriving (Eq,Show,Enum)
instance SchemaType THitPolicy where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType THitPolicy where
    acceptingParser =  do literal "UNIQUE"; return THitPolicy_UNIQUE
                      `onFail` do literal "FIRST"; return THitPolicy_FIRST
                      `onFail` do literal "PRIORITY"; return THitPolicy_PRIORITY
                      `onFail` do literal "ANY"; return THitPolicy_ANY
                      `onFail` do literal "COLLECT"; return THitPolicy_COLLECT
                      `onFail` do literal "RULE ORDER"; return THitPolicy_RULE_ORDER
                      `onFail` do literal "OUTPUT ORDER"; return THitPolicy_OUTPUT_ORDER
                      
    simpleTypeText THitPolicy_UNIQUE = "UNIQUE"
    simpleTypeText THitPolicy_FIRST = "FIRST"
    simpleTypeText THitPolicy_PRIORITY = "PRIORITY"
    simpleTypeText THitPolicy_ANY = "ANY"
    simpleTypeText THitPolicy_COLLECT = "COLLECT"
    simpleTypeText THitPolicy_RULE_ORDER = "RULE ORDER"
    simpleTypeText THitPolicy_OUTPUT_ORDER = "OUTPUT ORDER"
 
data TBuiltinAggregator
    = TBuiltinAggregator_SUM
    | TBuiltinAggregator_COUNT
    | TBuiltinAggregator_MIN
    | TBuiltinAggregator_MAX
    deriving (Eq,Show,Enum)
instance SchemaType TBuiltinAggregator where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TBuiltinAggregator where
    acceptingParser =  do literal "SUM"; return TBuiltinAggregator_SUM
                      `onFail` do literal "COUNT"; return TBuiltinAggregator_COUNT
                      `onFail` do literal "MIN"; return TBuiltinAggregator_MIN
                      `onFail` do literal "MAX"; return TBuiltinAggregator_MAX
                      
    simpleTypeText TBuiltinAggregator_SUM = "SUM"
    simpleTypeText TBuiltinAggregator_COUNT = "COUNT"
    simpleTypeText TBuiltinAggregator_MIN = "MIN"
    simpleTypeText TBuiltinAggregator_MAX = "MAX"
 
data TDecisionTableOrientation
    = TDecisionTableOrientation_Rule'as'Row
    | TDecisionTableOrientation_Rule'as'Column
    | TDecisionTableOrientation_CrossTable
    deriving (Eq,Show,Enum)
instance SchemaType TDecisionTableOrientation where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TDecisionTableOrientation where
    acceptingParser =  do literal "Rule-as-Row"; return TDecisionTableOrientation_Rule'as'Row
                      `onFail` do literal "Rule-as-Column"; return TDecisionTableOrientation_Rule'as'Column
                      `onFail` do literal "CrossTable"; return TDecisionTableOrientation_CrossTable
                      
    simpleTypeText TDecisionTableOrientation_Rule'as'Row = "Rule-as-Row"
    simpleTypeText TDecisionTableOrientation_Rule'as'Column = "Rule-as-Column"
    simpleTypeText TDecisionTableOrientation_CrossTable = "CrossTable"
 
data TImportedValues = TImportedValues
        { tImportedValues_id :: Maybe Xsd.ID
        , tImportedValues_label :: Maybe Xsd.XsdString
        , tImportedValues_name :: Xsd.XsdString
        , tImportedValues_namespace :: Xsd.AnyURI
        , tImportedValues_locationURI :: Maybe Xsd.AnyURI
        , tImportedValues_importType :: Xsd.AnyURI
        , tImportedValues_expressionLanguage :: Maybe Xsd.AnyURI
        , tImportedValues_description :: Maybe Xsd.XsdString
        , tImportedValues_extensionElements :: Maybe ExtensionElements
        , tImportedValues_importedElement :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType TImportedValues where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        a3 <- getAttribute "namespace" e pos
        a4 <- optional $ getAttribute "locationURI" e pos
        a5 <- getAttribute "importType" e pos
        a6 <- optional $ getAttribute "expressionLanguage" e pos
        commit $ interior e $ return (TImportedValues a0 a1 a2 a3 a4 a5 a6)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` parseSchemaType "importedElement"
    schemaTypeToXML s x@TImportedValues{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tImportedValues_id x
                       , maybe [] (toXMLAttribute "label") $ tImportedValues_label x
                       , toXMLAttribute "name" $ tImportedValues_name x
                       , toXMLAttribute "namespace" $ tImportedValues_namespace x
                       , maybe [] (toXMLAttribute "locationURI") $ tImportedValues_locationURI x
                       , toXMLAttribute "importType" $ tImportedValues_importType x
                       , maybe [] (toXMLAttribute "expressionLanguage") $ tImportedValues_expressionLanguage x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tImportedValues_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tImportedValues_extensionElements x
            , schemaTypeToXML "importedElement" $ tImportedValues_importedElement x
            ]
instance Extension TImportedValues TImport where
    supertype (TImportedValues a0 a1 a2 a3 a4 a5 a6 e0 e1 e2) =
               TImport a0 a1 a2 a3 a4 a5 e0 e1
instance Extension TImportedValues TNamedElement where
    supertype = (supertype :: TImport -> TNamedElement)
              . (supertype :: TImportedValues -> TImport)
              
instance Extension TImportedValues TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TImport -> TNamedElement)
              . (supertype :: TImportedValues -> TImport)
              
 
elementArtifact :: XMLParser TArtifact
elementArtifact = fmap supertype elementAssociation
                  `onFail`
                  fmap supertype elementTextAnnotation
                  `onFail`
                  fmap supertype elementGroup
                  `onFail` fail "Parse failed when expecting an element in the substitution group for\n\
\    <artifact>,\n\
\  namely one of:\n\
\<association>, <textAnnotation>, <group>"
elementToXMLArtifact :: TArtifact -> [Content ()]
elementToXMLArtifact = schemaTypeToXML "artifact"
 
data TArtifact = TArtifact
        { tArtifact_id :: Maybe Xsd.ID
        , tArtifact_label :: Maybe Xsd.XsdString
        , tArtifact_description :: Maybe Xsd.XsdString
        , tArtifact_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TArtifact where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TArtifact a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TArtifact{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tArtifact_id x
                       , maybe [] (toXMLAttribute "label") $ tArtifact_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tArtifact_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tArtifact_extensionElements x
            ]
instance Extension TArtifact TDMNElement where
    supertype (TArtifact a0 a1 e0 e1) =
               TDMNElement a0 a1 e0 e1
 
elementGroup :: XMLParser TGroup
elementGroup = parseSchemaType "group"
elementToXMLGroup :: TGroup -> [Content ()]
elementToXMLGroup = schemaTypeToXML "group"
 
data TGroup = TGroup
        { tGroup_id :: Maybe Xsd.ID
        , tGroup_label :: Maybe Xsd.XsdString
        , tGroup_name :: Maybe Xsd.XsdString
        , tGroup_description :: Maybe Xsd.XsdString
        , tGroup_extensionElements :: Maybe ExtensionElements
        }
        deriving (Eq,Show)
instance SchemaType TGroup where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "name" e pos
        commit $ interior e $ return (TGroup a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
    schemaTypeToXML s x@TGroup{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tGroup_id x
                       , maybe [] (toXMLAttribute "label") $ tGroup_label x
                       , maybe [] (toXMLAttribute "name") $ tGroup_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tGroup_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tGroup_extensionElements x
            ]
instance Extension TGroup TArtifact where
    supertype (TGroup a0 a1 a2 e0 e1) =
               TArtifact a0 a1 e0 e1
instance Extension TGroup TDMNElement where
    supertype = (supertype :: TArtifact -> TDMNElement)
              . (supertype :: TGroup -> TArtifact)
              
 
elementTextAnnotation :: XMLParser TTextAnnotation
elementTextAnnotation = parseSchemaType "textAnnotation"
elementToXMLTextAnnotation :: TTextAnnotation -> [Content ()]
elementToXMLTextAnnotation = schemaTypeToXML "textAnnotation"
 
data TTextAnnotation = TTextAnnotation
        { tTextAnnotation_id :: Maybe Xsd.ID
        , tTextAnnotation_label :: Maybe Xsd.XsdString
        , tTextAnnotation_textFormat :: Maybe Xsd.XsdString
        , tTextAnnotation_description :: Maybe Xsd.XsdString
        , tTextAnnotation_extensionElements :: Maybe ExtensionElements
        , tTextAnnotation_text :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType TTextAnnotation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "textFormat" e pos
        commit $ interior e $ return (TTextAnnotation a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "text")
    schemaTypeToXML s x@TTextAnnotation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tTextAnnotation_id x
                       , maybe [] (toXMLAttribute "label") $ tTextAnnotation_label x
                       , maybe [] (toXMLAttribute "textFormat") $ tTextAnnotation_textFormat x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tTextAnnotation_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tTextAnnotation_extensionElements x
            , maybe [] (schemaTypeToXML "text") $ tTextAnnotation_text x
            ]
instance Extension TTextAnnotation TArtifact where
    supertype (TTextAnnotation a0 a1 a2 e0 e1 e2) =
               TArtifact a0 a1 e0 e1
instance Extension TTextAnnotation TDMNElement where
    supertype = (supertype :: TArtifact -> TDMNElement)
              . (supertype :: TTextAnnotation -> TArtifact)
              
 
elementAssociation :: XMLParser TAssociation
elementAssociation = parseSchemaType "association"
elementToXMLAssociation :: TAssociation -> [Content ()]
elementToXMLAssociation = schemaTypeToXML "association"
 
data TAssociation = TAssociation
        { tAssociation_id :: Maybe Xsd.ID
        , tAssociation_label :: Maybe Xsd.XsdString
        , tAssociation_associationDirection :: Maybe TAssociationDirection
        , tAssociation_description :: Maybe Xsd.XsdString
        , tAssociation_extensionElements :: Maybe ExtensionElements
        , tAssociation_sourceRef :: TDMNElementReference
        , tAssociation_targetRef :: TDMNElementReference
        }
        deriving (Eq,Show)
instance SchemaType TAssociation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "associationDirection" e pos
        commit $ interior e $ return (TAssociation a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` parseSchemaType "sourceRef"
            `apply` parseSchemaType "targetRef"
    schemaTypeToXML s x@TAssociation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tAssociation_id x
                       , maybe [] (toXMLAttribute "label") $ tAssociation_label x
                       , maybe [] (toXMLAttribute "associationDirection") $ tAssociation_associationDirection x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tAssociation_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tAssociation_extensionElements x
            , schemaTypeToXML "sourceRef" $ tAssociation_sourceRef x
            , schemaTypeToXML "targetRef" $ tAssociation_targetRef x
            ]
instance Extension TAssociation TArtifact where
    supertype (TAssociation a0 a1 a2 e0 e1 e2 e3) =
               TArtifact a0 a1 e0 e1
instance Extension TAssociation TDMNElement where
    supertype = (supertype :: TArtifact -> TDMNElement)
              . (supertype :: TAssociation -> TArtifact)
              
 
data TAssociationDirection
    = TAssociationDirection_None
    | TAssociationDirection_One
    | TAssociationDirection_Both
    deriving (Eq,Show,Enum)
instance SchemaType TAssociationDirection where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TAssociationDirection where
    acceptingParser =  do literal "None"; return TAssociationDirection_None
                      `onFail` do literal "One"; return TAssociationDirection_One
                      `onFail` do literal "Both"; return TAssociationDirection_Both
                      
    simpleTypeText TAssociationDirection_None = "None"
    simpleTypeText TAssociationDirection_One = "One"
    simpleTypeText TAssociationDirection_Both = "Both"
 
elementContext :: XMLParser TContext
elementContext = parseSchemaType "context"
elementToXMLContext :: TContext -> [Content ()]
elementToXMLContext = schemaTypeToXML "context"
 
data TContext = TContext
        { tContext_id :: Maybe Xsd.ID
        , tContext_label :: Maybe Xsd.XsdString
        , tContext_typeRef :: Maybe Xsd.XsdString
        , tContext_description :: Maybe Xsd.XsdString
        , tContext_extensionElements :: Maybe ExtensionElements
        , tContext_contextEntry :: [TContextEntry]
        }
        deriving (Eq,Show)
instance SchemaType TContext where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TContext a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (elementContextEntry)
    schemaTypeToXML s x@TContext{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tContext_id x
                       , maybe [] (toXMLAttribute "label") $ tContext_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tContext_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tContext_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tContext_extensionElements x
            , concatMap (elementToXMLContextEntry) $ tContext_contextEntry x
            ]
instance Extension TContext TExpression where
    supertype (TContext a0 a1 a2 e0 e1 e2) =
               TExpression a0 a1 a2 e0 e1
instance Extension TContext TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TContext -> TExpression)
              
 
elementContextEntry :: XMLParser TContextEntry
elementContextEntry = parseSchemaType "contextEntry"
elementToXMLContextEntry :: TContextEntry -> [Content ()]
elementToXMLContextEntry = schemaTypeToXML "contextEntry"
 
data TContextEntry = TContextEntry
        { tContextEntry_id :: Maybe Xsd.ID
        , tContextEntry_label :: Maybe Xsd.XsdString
        , tContextEntry_description :: Maybe Xsd.XsdString
        , tContextEntry_extensionElements :: Maybe ExtensionElements
        , tContextEntry_variable :: Maybe TInformationItem
        , tContextEntry_expression :: TExpression
        }
        deriving (Eq,Show)
instance SchemaType TContextEntry where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        commit $ interior e $ return (TContextEntry a0 a1)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "variable")
            `apply` elementExpression
    schemaTypeToXML s x@TContextEntry{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tContextEntry_id x
                       , maybe [] (toXMLAttribute "label") $ tContextEntry_label x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tContextEntry_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tContextEntry_extensionElements x
            , maybe [] (schemaTypeToXML "variable") $ tContextEntry_variable x
            , elementToXMLExpression $ tContextEntry_expression x
            ]
instance Extension TContextEntry TDMNElement where
    supertype (TContextEntry a0 a1 e0 e1 e2 e3) =
               TDMNElement a0 a1 e0 e1
 
elementFunctionDefinition :: XMLParser TFunctionDefinition
elementFunctionDefinition = parseSchemaType "functionDefinition"
elementToXMLFunctionDefinition :: TFunctionDefinition -> [Content ()]
elementToXMLFunctionDefinition = schemaTypeToXML "functionDefinition"
 
data TFunctionDefinition = TFunctionDefinition
        { tFunctionDefinition_id :: Maybe Xsd.ID
        , tFunctionDefinition_label :: Maybe Xsd.XsdString
        , tFunctionDefinition_typeRef :: Maybe Xsd.XsdString
        , tFunctionDefinition_kind :: Maybe TFunctionKind
        , tFunctionDefinition_description :: Maybe Xsd.XsdString
        , tFunctionDefinition_extensionElements :: Maybe ExtensionElements
        , tFunctionDefinition_formalParameter :: [TInformationItem]
        , tFunctionDefinition_expression :: Maybe TExpression
        }
        deriving (Eq,Show)
instance SchemaType TFunctionDefinition where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        a3 <- optional $ getAttribute "kind" e pos
        commit $ interior e $ return (TFunctionDefinition a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "formalParameter")
            `apply` optional (elementExpression)
    schemaTypeToXML s x@TFunctionDefinition{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tFunctionDefinition_id x
                       , maybe [] (toXMLAttribute "label") $ tFunctionDefinition_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tFunctionDefinition_typeRef x
                       , maybe [] (toXMLAttribute "kind") $ tFunctionDefinition_kind x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tFunctionDefinition_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tFunctionDefinition_extensionElements x
            , concatMap (schemaTypeToXML "formalParameter") $ tFunctionDefinition_formalParameter x
            , maybe [] (elementToXMLExpression) $ tFunctionDefinition_expression x
            ]
instance Extension TFunctionDefinition TExpression where
    supertype (TFunctionDefinition a0 a1 a2 a3 e0 e1 e2 e3) =
               TExpression a0 a1 a2 e0 e1
instance Extension TFunctionDefinition TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TFunctionDefinition -> TExpression)
              
 
data TFunctionKind
    = TFunctionKind_FEEL
    | TFunctionKind_Java
    | TFunctionKind_PMML
    deriving (Eq,Show,Enum)
instance SchemaType TFunctionKind where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TFunctionKind where
    acceptingParser =  do literal "FEEL"; return TFunctionKind_FEEL
                      `onFail` do literal "Java"; return TFunctionKind_Java
                      `onFail` do literal "PMML"; return TFunctionKind_PMML
                      
    simpleTypeText TFunctionKind_FEEL = "FEEL"
    simpleTypeText TFunctionKind_Java = "Java"
    simpleTypeText TFunctionKind_PMML = "PMML"
 
elementRelation :: XMLParser TRelation
elementRelation = parseSchemaType "relation"
elementToXMLRelation :: TRelation -> [Content ()]
elementToXMLRelation = schemaTypeToXML "relation"
 
data TRelation = TRelation
        { tRelation_id :: Maybe Xsd.ID
        , tRelation_label :: Maybe Xsd.XsdString
        , tRelation_typeRef :: Maybe Xsd.XsdString
        , tRelation_description :: Maybe Xsd.XsdString
        , tRelation_extensionElements :: Maybe ExtensionElements
        , tRelation_column :: [TInformationItem]
        , tRelation_row :: [TList]
        }
        deriving (Eq,Show)
instance SchemaType TRelation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TRelation a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (parseSchemaType "column")
            `apply` many (parseSchemaType "row")
    schemaTypeToXML s x@TRelation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tRelation_id x
                       , maybe [] (toXMLAttribute "label") $ tRelation_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tRelation_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tRelation_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tRelation_extensionElements x
            , concatMap (schemaTypeToXML "column") $ tRelation_column x
            , concatMap (schemaTypeToXML "row") $ tRelation_row x
            ]
instance Extension TRelation TExpression where
    supertype (TRelation a0 a1 a2 e0 e1 e2 e3) =
               TExpression a0 a1 a2 e0 e1
instance Extension TRelation TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TRelation -> TExpression)
              
 
elementList :: XMLParser TList
elementList = parseSchemaType "list"
elementToXMLList :: TList -> [Content ()]
elementToXMLList = schemaTypeToXML "list"
 
data TList = TList
        { tList_id :: Maybe Xsd.ID
        , tList_label :: Maybe Xsd.XsdString
        , tList_typeRef :: Maybe Xsd.XsdString
        , tList_description :: Maybe Xsd.XsdString
        , tList_extensionElements :: Maybe ExtensionElements
        , tList_expression :: [TExpression]
        }
        deriving (Eq,Show)
instance SchemaType TList where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        commit $ interior e $ return (TList a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` many (elementExpression)
    schemaTypeToXML s x@TList{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tList_id x
                       , maybe [] (toXMLAttribute "label") $ tList_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tList_typeRef x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tList_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tList_extensionElements x
            , concatMap (elementToXMLExpression) $ tList_expression x
            ]
instance Extension TList TExpression where
    supertype (TList a0 a1 a2 e0 e1 e2) =
               TExpression a0 a1 a2 e0 e1
instance Extension TList TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TList -> TExpression)
              
 
data TUnaryTests = TUnaryTests
        { tUnaryTests_id :: Maybe Xsd.ID
        , tUnaryTests_label :: Maybe Xsd.XsdString
        , tUnaryTests_typeRef :: Maybe Xsd.XsdString
        , tUnaryTests_expressionLanguage :: Maybe Xsd.AnyURI
        , tUnaryTests_description :: Maybe Xsd.XsdString
        , tUnaryTests_extensionElements :: Maybe ExtensionElements
        , tUnaryTests_text :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType TUnaryTests where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- optional $ getAttribute "typeRef" e pos
        a3 <- optional $ getAttribute "expressionLanguage" e pos
        commit $ interior e $ return (TUnaryTests a0 a1 a2 a3)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` parseSchemaType "text"
    schemaTypeToXML s x@TUnaryTests{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tUnaryTests_id x
                       , maybe [] (toXMLAttribute "label") $ tUnaryTests_label x
                       , maybe [] (toXMLAttribute "typeRef") $ tUnaryTests_typeRef x
                       , maybe [] (toXMLAttribute "expressionLanguage") $ tUnaryTests_expressionLanguage x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tUnaryTests_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tUnaryTests_extensionElements x
            , schemaTypeToXML "text" $ tUnaryTests_text x
            ]
instance Extension TUnaryTests TExpression where
    supertype (TUnaryTests a0 a1 a2 a3 e0 e1 e2) =
               TExpression a0 a1 a2 e0 e1
instance Extension TUnaryTests TDMNElement where
    supertype = (supertype :: TExpression -> TDMNElement)
              . (supertype :: TUnaryTests -> TExpression)
              
 
elementDecisionService :: XMLParser TDecisionService
elementDecisionService = parseSchemaType "decisionService"
elementToXMLDecisionService :: TDecisionService -> [Content ()]
elementToXMLDecisionService = schemaTypeToXML "decisionService"
 
data TDecisionService = TDecisionService
        { tDecisionService_id :: Maybe Xsd.ID
        , tDecisionService_label :: Maybe Xsd.XsdString
        , tDecisionService_name :: Xsd.XsdString
        , tDecisionService_description :: Maybe Xsd.XsdString
        , tDecisionService_extensionElements :: Maybe ExtensionElements
        , tDecisionService_variable :: Maybe TInformationItem
        , tDecisionService_outputDecision :: [TDMNElementReference]
        , tDecisionService_encapsulatedDecision :: [TDMNElementReference]
        , tDecisionService_inputDecision :: [TDMNElementReference]
        , tDecisionService_inputData :: [TDMNElementReference]
        }
        deriving (Eq,Show)
instance SchemaType TDecisionService where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "id" e pos
        a1 <- optional $ getAttribute "label" e pos
        a2 <- getAttribute "name" e pos
        commit $ interior e $ return (TDecisionService a0 a1 a2)
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "extensionElements")
            `apply` optional (parseSchemaType "variable")
            `apply` many (parseSchemaType "outputDecision")
            `apply` many (parseSchemaType "encapsulatedDecision")
            `apply` many (parseSchemaType "inputDecision")
            `apply` many (parseSchemaType "inputData")
    schemaTypeToXML s x@TDecisionService{} =
        toXMLElement s [ maybe [] (toXMLAttribute "id") $ tDecisionService_id x
                       , maybe [] (toXMLAttribute "label") $ tDecisionService_label x
                       , toXMLAttribute "name" $ tDecisionService_name x
                       ]
            [ maybe [] (schemaTypeToXML "description") $ tDecisionService_description x
            , maybe [] (schemaTypeToXML "extensionElements") $ tDecisionService_extensionElements x
            , maybe [] (schemaTypeToXML "variable") $ tDecisionService_variable x
            , concatMap (schemaTypeToXML "outputDecision") $ tDecisionService_outputDecision x
            , concatMap (schemaTypeToXML "encapsulatedDecision") $ tDecisionService_encapsulatedDecision x
            , concatMap (schemaTypeToXML "inputDecision") $ tDecisionService_inputDecision x
            , concatMap (schemaTypeToXML "inputData") $ tDecisionService_inputData x
            ]
instance Extension TDecisionService TInvocable where
    supertype (TDecisionService a0 a1 a2 e0 e1 e2 e3 e4 e5 e6) =
               TInvocable a0 a1 a2 e0 e1 e2
instance Extension TDecisionService TDRGElement where
    supertype = (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TDecisionService -> TInvocable)
              
instance Extension TDecisionService TNamedElement where
    supertype = (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TDecisionService -> TInvocable)
              
instance Extension TDecisionService TDMNElement where
    supertype = (supertype :: TNamedElement -> TDMNElement)
              . (supertype :: TDRGElement -> TNamedElement)
              . (supertype :: TInvocable -> TDRGElement)
              . (supertype :: TDecisionService -> TInvocable)
              
