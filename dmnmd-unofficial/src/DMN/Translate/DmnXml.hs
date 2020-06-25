module DMN.Translate.DmnXml where

import DMN13'xsd
import Text.XML.HaXml
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Posn

parseDMN :: FilePath -> IO (Either String TDefinitions)
parseDMN fname = do
    xmlText <- readFile fname
    let (Document _ _ root _) = xmlParse "(No Document)" xmlText
    pure $ fst $ runParser elementDefinitions [CElem root noPos]