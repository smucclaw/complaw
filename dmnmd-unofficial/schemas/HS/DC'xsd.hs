{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Xsd'DC'xsd
  ( module Xsd'DC'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementColor :: XMLParser Color
elementColor = parseSchemaType "Color"
elementToXMLColor :: Color -> [Content ()]
elementToXMLColor = schemaTypeToXML "Color"
 
elementPoint :: XMLParser Point
elementPoint = parseSchemaType "Point"
elementToXMLPoint :: Point -> [Content ()]
elementToXMLPoint = schemaTypeToXML "Point"
 
elementBounds :: XMLParser Bounds
elementBounds = parseSchemaType "Bounds"
elementToXMLBounds :: Bounds -> [Content ()]
elementToXMLBounds = schemaTypeToXML "Bounds"
 
elementDimension :: XMLParser Dimension
elementDimension = parseSchemaType "Dimension"
elementToXMLDimension :: Dimension -> [Content ()]
elementToXMLDimension = schemaTypeToXML "Dimension"
 
-- | Color is a data type that represents a color value in the 
--   RGB format.
data Color = Color
        { color_red :: Rgb
        , color_green :: Rgb
        , color_blue :: Rgb
        }
        deriving (Eq,Show)
instance SchemaType Color where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "red" e pos
        a1 <- getAttribute "green" e pos
        a2 <- getAttribute "blue" e pos
        commit $ interior e $ return (Color a0 a1 a2)
    schemaTypeToXML s x@Color{} =
        toXMLElement s [ toXMLAttribute "red" $ color_red x
                       , toXMLAttribute "green" $ color_green x
                       , toXMLAttribute "blue" $ color_blue x
                       ]
            []
 
newtype Rgb = Rgb Xsd.Int deriving (Eq,Show)
instance Restricts Rgb Xsd.Int where
    restricts (Rgb x) = x
instance SchemaType Rgb where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Rgb x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Rgb where
    acceptingParser = fmap Rgb acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs (Just 0) (Just 255)))
    simpleTypeText (Rgb x) = simpleTypeText x
 
-- | A Point specifies an location in some x-y coordinate 
--   system.
data Point = Point
        { point_x :: Xsd.Double
        , point_y :: Xsd.Double
        }
        deriving (Eq,Show)
instance SchemaType Point where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "x" e pos
        a1 <- getAttribute "y" e pos
        commit $ interior e $ return (Point a0 a1)
    schemaTypeToXML s x@Point{} =
        toXMLElement s [ toXMLAttribute "x" $ point_x x
                       , toXMLAttribute "y" $ point_y x
                       ]
            []
 
-- | Dimension specifies two lengths (width and height) along 
--   the x and y axes in some x-y coordinate system.
data Dimension = Dimension
        { dimension_width :: Xsd.Double
        , dimension_height :: Xsd.Double
        }
        deriving (Eq,Show)
instance SchemaType Dimension where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "width" e pos
        a1 <- getAttribute "height" e pos
        commit $ interior e $ return (Dimension a0 a1)
    schemaTypeToXML s x@Dimension{} =
        toXMLElement s [ toXMLAttribute "width" $ dimension_width x
                       , toXMLAttribute "height" $ dimension_height x
                       ]
            []
 
-- | Bounds specifies a rectangular area in some x-y coordinate 
--   system that is defined by a location (x and y) and a size 
--   (width and height).
data Bounds = Bounds
        { bounds_x :: Xsd.Double
        , bounds_y :: Xsd.Double
        , bounds_width :: Xsd.Double
        , bounds_height :: Xsd.Double
        }
        deriving (Eq,Show)
instance SchemaType Bounds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "x" e pos
        a1 <- getAttribute "y" e pos
        a2 <- getAttribute "width" e pos
        a3 <- getAttribute "height" e pos
        commit $ interior e $ return (Bounds a0 a1 a2 a3)
    schemaTypeToXML s x@Bounds{} =
        toXMLElement s [ toXMLAttribute "x" $ bounds_x x
                       , toXMLAttribute "y" $ bounds_y x
                       , toXMLAttribute "width" $ bounds_width x
                       , toXMLAttribute "height" $ bounds_height x
                       ]
            []
 
-- | AlignmentKind enumerates the possible options for alignment 
--   for layout purposes.
data AlignmentKind
    = AlignmentKind_Start
    | AlignmentKind_End
    | AlignmentKind_Center
    deriving (Eq,Show,Enum)
instance SchemaType AlignmentKind where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AlignmentKind where
    acceptingParser =  do literal "start"; return AlignmentKind_Start
                      `onFail` do literal "end"; return AlignmentKind_End
                      `onFail` do literal "center"; return AlignmentKind_Center
                      
    simpleTypeText AlignmentKind_Start = "start"
    simpleTypeText AlignmentKind_End = "end"
    simpleTypeText AlignmentKind_Center = "center"
 
-- | KnownColor is an enumeration of 17 known colors.
data KnownColor
    = KnownColor_Maroon
      -- ^ a color with a value of #800000
    | KnownColor_Red
      -- ^ a color with a value of #FF0000
    | KnownColor_Orange
      -- ^ a color with a value of #FFA500
    | KnownColor_Yellow
      -- ^ a color with a value of #FFFF00
    | KnownColor_Olive
      -- ^ a color with a value of #808000
    | KnownColor_Purple
      -- ^ a color with a value of #800080
    | KnownColor_Fuchsia
      -- ^ a color with a value of #FF00FF
    | KnownColor_White
      -- ^ a color with a value of #FFFFFF
    | KnownColor_Lime
      -- ^ a color with a value of #00FF00
    | KnownColor_Green
      -- ^ a color with a value of #008000
    | KnownColor_Navy
      -- ^ a color with a value of #000080
    | KnownColor_Blue
      -- ^ a color with a value of #0000FF
    | KnownColor_Aqua
      -- ^ a color with a value of #00FFFF
    | KnownColor_Teal
      -- ^ a color with a value of #008080
    | KnownColor_Black
      -- ^ a color with a value of #000000
    | KnownColor_Silver
      -- ^ a color with a value of #C0C0C0
    | KnownColor_Gray
      -- ^ a color with a value of #808080
    deriving (Eq,Show,Enum)
instance SchemaType KnownColor where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType KnownColor where
    acceptingParser =  do literal "maroon"; return KnownColor_Maroon
                      `onFail` do literal "red"; return KnownColor_Red
                      `onFail` do literal "orange"; return KnownColor_Orange
                      `onFail` do literal "yellow"; return KnownColor_Yellow
                      `onFail` do literal "olive"; return KnownColor_Olive
                      `onFail` do literal "purple"; return KnownColor_Purple
                      `onFail` do literal "fuchsia"; return KnownColor_Fuchsia
                      `onFail` do literal "white"; return KnownColor_White
                      `onFail` do literal "lime"; return KnownColor_Lime
                      `onFail` do literal "green"; return KnownColor_Green
                      `onFail` do literal "navy"; return KnownColor_Navy
                      `onFail` do literal "blue"; return KnownColor_Blue
                      `onFail` do literal "aqua"; return KnownColor_Aqua
                      `onFail` do literal "teal"; return KnownColor_Teal
                      `onFail` do literal "black"; return KnownColor_Black
                      `onFail` do literal "silver"; return KnownColor_Silver
                      `onFail` do literal "gray"; return KnownColor_Gray
                      
    simpleTypeText KnownColor_Maroon = "maroon"
    simpleTypeText KnownColor_Red = "red"
    simpleTypeText KnownColor_Orange = "orange"
    simpleTypeText KnownColor_Yellow = "yellow"
    simpleTypeText KnownColor_Olive = "olive"
    simpleTypeText KnownColor_Purple = "purple"
    simpleTypeText KnownColor_Fuchsia = "fuchsia"
    simpleTypeText KnownColor_White = "white"
    simpleTypeText KnownColor_Lime = "lime"
    simpleTypeText KnownColor_Green = "green"
    simpleTypeText KnownColor_Navy = "navy"
    simpleTypeText KnownColor_Blue = "blue"
    simpleTypeText KnownColor_Aqua = "aqua"
    simpleTypeText KnownColor_Teal = "teal"
    simpleTypeText KnownColor_Black = "black"
    simpleTypeText KnownColor_Silver = "silver"
    simpleTypeText KnownColor_Gray = "gray"
