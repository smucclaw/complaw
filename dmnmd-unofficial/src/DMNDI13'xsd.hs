{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module DMNDI13'xsd
  ( module DMNDI13'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import DC'xsd as Dc
import DI'xsd as Di
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementDMNDI :: XMLParser DMNDI
elementDMNDI = parseSchemaType "DMNDI"
elementToXMLDMNDI :: DMNDI -> [Content ()]
elementToXMLDMNDI = schemaTypeToXML "DMNDI"
 
elementDMNDiagram :: XMLParser DMNDiagram
elementDMNDiagram = parseSchemaType "DMNDiagram"
elementToXMLDMNDiagram :: DMNDiagram -> [Content ()]
elementToXMLDMNDiagram = schemaTypeToXML "DMNDiagram"
 
-- | This element should never be instantiated directly, but 
--   rather concrete implementation should. It is placed there 
--   only to be referred in the sequence
elementDMNDiagramElement :: XMLParser Di.DiagramElement
elementDMNDiagramElement = parseSchemaType "DMNDiagramElement"
elementToXMLDMNDiagramElement :: Di.DiagramElement -> [Content ()]
elementToXMLDMNDiagramElement = schemaTypeToXML "DMNDiagramElement"
 
elementDMNShape :: XMLParser DMNShape
elementDMNShape = parseSchemaType "DMNShape"
elementToXMLDMNShape :: DMNShape -> [Content ()]
elementToXMLDMNShape = schemaTypeToXML "DMNShape"
 
elementDMNEdge :: XMLParser DMNEdge
elementDMNEdge = parseSchemaType "DMNEdge"
elementToXMLDMNEdge :: DMNEdge -> [Content ()]
elementToXMLDMNEdge = schemaTypeToXML "DMNEdge"
 
elementDMNStyle :: XMLParser DMNStyle
elementDMNStyle = parseSchemaType "DMNStyle"
elementToXMLDMNStyle :: DMNStyle -> [Content ()]
elementToXMLDMNStyle = schemaTypeToXML "DMNStyle"
 
elementDMNLabel :: XMLParser DMNLabel
elementDMNLabel = parseSchemaType "DMNLabel"
elementToXMLDMNLabel :: DMNLabel -> [Content ()]
elementToXMLDMNLabel = schemaTypeToXML "DMNLabel"
 
elementDMNDecisionServiceDividerLine :: XMLParser DMNDecisionServiceDividerLine
elementDMNDecisionServiceDividerLine = parseSchemaType "DMNDecisionServiceDividerLine"
elementToXMLDMNDecisionServiceDividerLine :: DMNDecisionServiceDividerLine -> [Content ()]
elementToXMLDMNDecisionServiceDividerLine = schemaTypeToXML "DMNDecisionServiceDividerLine"
 
data DMNDI = DMNDI
        { dMNDI_dMNDiagram :: [DMNDiagram]
        , dMNDI_dMNStyle :: [DMNStyle]
        }
        deriving (Eq,Show)
instance SchemaType DMNDI where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DMNDI
            `apply` many (elementDMNDiagram)
            `apply` many (elementDMNStyle)
    schemaTypeToXML s x@DMNDI{} =
        toXMLElement s []
            [ concatMap (elementToXMLDMNDiagram) $ dMNDI_dMNDiagram x
            , concatMap (elementToXMLDMNStyle) $ dMNDI_dMNStyle x
            ]
 
data DMNDiagram = DMNDiagram
        { dMNDiagram_size :: Maybe Dc.Dimension
        , dMNDiagram_dMNDiagramElement :: [Di.DiagramElement]
          -- ^ This element should never be instantiated directly, but 
          --   rather concrete implementation should. It is placed there 
          --   only to be referred in the sequence
        }
        deriving (Eq,Show)
instance SchemaType DMNDiagram where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DMNDiagram
            `apply` optional (parseSchemaType "Size")
            `apply` many (elementDMNDiagramElement)
    schemaTypeToXML s x@DMNDiagram{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Size") $ dMNDiagram_size x
            , concatMap (elementToXMLDMNDiagramElement) $ dMNDiagram_dMNDiagramElement x
            ]
instance Extension DMNDiagram Di.Diagram where
    supertype (DMNDiagram e0 e1) =
               Diagram
 
data DMNShape = DMNShape
        { dMNShape_dmnElementRef :: Xsd.QName
        , dMNShape_isListedInputData :: Maybe Xsd.Boolean
        , dMNShape_isCollapsed :: Maybe Xsd.Boolean
        , dMNShape_dMNLabel :: Maybe DMNLabel
        , dMNShape_dMNDecisionServiceDividerLine :: Maybe DMNDecisionServiceDividerLine
        }
        deriving (Eq,Show)
instance SchemaType DMNShape where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "dmnElementRef" e pos
        a1 <- optional $ getAttribute "isListedInputData" e pos
        a2 <- optional $ getAttribute "isCollapsed" e pos
        commit $ interior e $ return (DMNShape a0 a1 a2)
            `apply` optional (elementDMNLabel)
            `apply` optional (elementDMNDecisionServiceDividerLine)
    schemaTypeToXML s x@DMNShape{} =
        toXMLElement s [ toXMLAttribute "dmnElementRef" $ dMNShape_dmnElementRef x
                       , maybe [] (toXMLAttribute "isListedInputData") $ dMNShape_isListedInputData x
                       , maybe [] (toXMLAttribute "isCollapsed") $ dMNShape_isCollapsed x
                       ]
            [ maybe [] (elementToXMLDMNLabel) $ dMNShape_dMNLabel x
            , maybe [] (elementToXMLDMNDecisionServiceDividerLine) $ dMNShape_dMNDecisionServiceDividerLine x
            ]
instance Extension DMNShape Di.Shape where
    supertype (DMNShape a0 a1 a2 e0 e1) =
               Shape
 
data DMNDecisionServiceDividerLine = DMNDecisionServiceDividerLine
        deriving (Eq,Show)
instance SchemaType DMNDecisionServiceDividerLine where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DMNDecisionServiceDividerLine
    schemaTypeToXML s x@DMNDecisionServiceDividerLine{} =
        toXMLElement s []
            []
instance Extension DMNDecisionServiceDividerLine Di.Edge where
    supertype (DMNDecisionServiceDividerLine) =
               Edge
 
data DMNEdge = DMNEdge
        { dMNEdge_dmnElementRef :: Xsd.QName
        , dMNEdge_sourceElement :: Maybe Xsd.QName
        , dMNEdge_targetElement :: Maybe Xsd.QName
        , dMNEdge_dMNLabel :: Maybe DMNLabel
        }
        deriving (Eq,Show)
instance SchemaType DMNEdge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "dmnElementRef" e pos
        a1 <- optional $ getAttribute "sourceElement" e pos
        a2 <- optional $ getAttribute "targetElement" e pos
        commit $ interior e $ return (DMNEdge a0 a1 a2)
            `apply` optional (elementDMNLabel)
    schemaTypeToXML s x@DMNEdge{} =
        toXMLElement s [ toXMLAttribute "dmnElementRef" $ dMNEdge_dmnElementRef x
                       , maybe [] (toXMLAttribute "sourceElement") $ dMNEdge_sourceElement x
                       , maybe [] (toXMLAttribute "targetElement") $ dMNEdge_targetElement x
                       ]
            [ maybe [] (elementToXMLDMNLabel) $ dMNEdge_dMNLabel x
            ]
instance Extension DMNEdge Di.Edge where
    supertype (DMNEdge a0 a1 a2 e0) =
               Edge
 
data DMNLabel = DMNLabel
        { dMNLabel_text :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType DMNLabel where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DMNLabel
            `apply` optional (parseSchemaType "Text")
    schemaTypeToXML s x@DMNLabel{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Text") $ dMNLabel_text x
            ]
instance Extension DMNLabel Di.Shape where
    supertype (DMNLabel e0) =
               Shape
 
data DMNStyle = DMNStyle
        { dMNStyle_fontFamily :: Maybe Xsd.XsdString
        , dMNStyle_fontSize :: Maybe Xsd.Double
        , dMNStyle_fontItalic :: Maybe Xsd.Boolean
        , dMNStyle_fontBold :: Maybe Xsd.Boolean
        , dMNStyle_fontUnderline :: Maybe Xsd.Boolean
        , dMNStyle_fontStrikeThrough :: Maybe Xsd.Boolean
        , dMNStyle_labelHorizontalAlignement :: Maybe Dc.AlignmentKind
        , dMNStyle_labelVerticalAlignment :: Maybe Dc.AlignmentKind
        , dMNStyle_fillColor :: Maybe Dc.Color
        , dMNStyle_strokeColor :: Maybe Dc.Color
        , dMNStyle_fontColor :: Maybe Dc.Color
        }
        deriving (Eq,Show)
instance SchemaType DMNStyle where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "fontFamily" e pos
        a1 <- optional $ getAttribute "fontSize" e pos
        a2 <- optional $ getAttribute "fontItalic" e pos
        a3 <- optional $ getAttribute "fontBold" e pos
        a4 <- optional $ getAttribute "fontUnderline" e pos
        a5 <- optional $ getAttribute "fontStrikeThrough" e pos
        a6 <- optional $ getAttribute "labelHorizontalAlignement" e pos
        a7 <- optional $ getAttribute "labelVerticalAlignment" e pos
        commit $ interior e $ return (DMNStyle a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` optional (parseSchemaType "FillColor")
            `apply` optional (parseSchemaType "StrokeColor")
            `apply` optional (parseSchemaType "FontColor")
    schemaTypeToXML s x@DMNStyle{} =
        toXMLElement s [ maybe [] (toXMLAttribute "fontFamily") $ dMNStyle_fontFamily x
                       , maybe [] (toXMLAttribute "fontSize") $ dMNStyle_fontSize x
                       , maybe [] (toXMLAttribute "fontItalic") $ dMNStyle_fontItalic x
                       , maybe [] (toXMLAttribute "fontBold") $ dMNStyle_fontBold x
                       , maybe [] (toXMLAttribute "fontUnderline") $ dMNStyle_fontUnderline x
                       , maybe [] (toXMLAttribute "fontStrikeThrough") $ dMNStyle_fontStrikeThrough x
                       , maybe [] (toXMLAttribute "labelHorizontalAlignement") $ dMNStyle_labelHorizontalAlignement x
                       , maybe [] (toXMLAttribute "labelVerticalAlignment") $ dMNStyle_labelVerticalAlignment x
                       ]
            [ maybe [] (schemaTypeToXML "FillColor") $ dMNStyle_fillColor x
            , maybe [] (schemaTypeToXML "StrokeColor") $ dMNStyle_strokeColor x
            , maybe [] (schemaTypeToXML "FontColor") $ dMNStyle_fontColor x
            ]
instance Extension DMNStyle Di.Style where
    supertype (DMNStyle a0 a1 a2 a3 a4 a5 a6 a7 e0 e1 e2) =
               Style
