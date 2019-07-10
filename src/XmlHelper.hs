

module XmlHelper where

import qualified Debug.Trace       as Debug
import           Text.XML.HXT.Core

da :: (Show s, ArrowXml a) => String -> a s s
da header = arr (\o -> Debug.trace (header ++ show o) o)

isType :: ArrowXml a => String -> a XmlTree XmlTree
isType t = hasAttrValue "typename" (== t) <+> hasAttrValue "profiletypename" (== t) <+> hasAttrValue "type" (== t)

isTypeF :: ArrowXml a => (String -> Bool) -> a XmlTree XmlTree
isTypeF f = hasAttrValue "typename" f <+> hasAttrValue "profiletypename" f <+> hasAttrValue "type" f

getType :: ArrowXml a => a XmlTree String
getType = getAttrValue "typename" <+> getAttrValue "profiletypename" <+> getAttrValue "type"

getBatScribeValue :: ArrowXml a => a XmlTree String
getBatScribeValue = single ((this /> getText) <+> getAttrValue "value")

