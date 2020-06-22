{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module DI'xsd
  ( module DI'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import DC'xsd as Dc
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
-- | The Diagram Interchange (DI) package enables interchange of 
--   graphical information that language users have control 
--   over, such as position of nodes and line routing points. 
--   Language specifications specialize elements of DI to define 
--   diagram interchange elements for a language.
 
-- | This element should never be instantiated directly, but 
--   rather concrete implementation should. It is placed there 
--   only to be referred in the sequence
elementStyle :: XMLParser Style
elementStyle = parseSchemaType "Style"
elementToXMLStyle :: Style -> [Content ()]
elementToXMLStyle = schemaTypeToXML "Style"
 
-- | DiagramElement is the abstract super type of all elements 
--   in diagrams, including diagrams themselves. When contained 
--   in a diagram, diagram elements are laid out relative to the 
--   diagram's origin.
data DiagramElement
        = DiagramElement_Edge Edge
        | DiagramElement_Shape Shape
        | DiagramElement_Diagram Diagram
        
        deriving (Eq,Show)
instance SchemaType DiagramElement where
    parseSchemaType s = do
        (fmap DiagramElement_Edge $ parseSchemaType s)
        `onFail`
        (fmap DiagramElement_Shape $ parseSchemaType s)
        `onFail`
        (fmap DiagramElement_Diagram $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of DiagramElement,\n\
\  namely one of:\n\
\Edge,Shape,Diagram"
    schemaTypeToXML _s (DiagramElement_Edge x) = schemaTypeToXML "edge" x
    schemaTypeToXML _s (DiagramElement_Shape x) = schemaTypeToXML "shape" x
    schemaTypeToXML _s (DiagramElement_Diagram x) = schemaTypeToXML "diagram" x
 
--  (There are no subtypes defined for this abstract type.)
data Diagram = Diagram deriving (Eq,Show)
instance SchemaType Diagram where
    parseSchemaType s = fail "Parse failed when expecting an extension type of Diagram:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
instance Extension Diagram DiagramElement where
    supertype v = DiagramElement_Diagram v
 
--  (There are no subtypes defined for this abstract type.)
data Shape = Shape deriving (Eq,Show)
instance SchemaType Shape where
    parseSchemaType s = fail "Parse failed when expecting an extension type of Shape:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
instance Extension Shape DiagramElement where
    supertype v = DiagramElement_Shape v
 
--  (There are no subtypes defined for this abstract type.)
data Edge = Edge deriving (Eq,Show)
instance SchemaType Edge where
    parseSchemaType s = fail "Parse failed when expecting an extension type of Edge:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
instance Extension Edge DiagramElement where
    supertype v = DiagramElement_Edge v
 
-- | Style contains formatting properties that affect the 
--   appearance or style of diagram elements, including diagram 
--   themselves.
--  (There are no subtypes defined for this abstract type.)
data Style = Style deriving (Eq,Show)
instance SchemaType Style where
    parseSchemaType s = fail "Parse failed when expecting an extension type of Style:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
