{-# LANGUAGE Arrows     #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Arrow
import           Control.Category
import           Prelude            hiding ((.))
import           Safe
import           System.Environment
import           Text.XML.HXT.Core

unsplit :: Arrow a => (b -> c -> d) ->  a (b,c) d
unsplit = arr . uncurry

split :: Arrow a => a b (b,b)
split = arr (\x -> (x,x))

liftA2 :: Arrow a => (c2 -> c1 -> c) -> a b c2 -> a b c1 -> a b c
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

data ModelGroup = ModelGroup {
  name       :: String,
  modelCount :: Integer,
  stats      ::  Stats
} deriving Show

data Stats = Stats {
  move       :: String,
  ws         :: String,
  bs         :: String,
  strength   :: String,
  toughness  :: String,
  wounds     :: String,
  attacks    :: String,
  leadership :: String,
  save       :: String
} deriving Show

newtype Unit = Unit [ModelGroup] deriving Show

findUnits :: ArrowXml a => a XmlTree XmlTree
findUnits = multi (isElem >>> hasName "selection" >>> hasAttrValue "type" (== "unit"))

findModels :: ArrowXml a => a XmlTree XmlTree
findModels = multi (isElem >>> hasName "selection" >>> hasAttrValue "type" (== "model"))

getStat :: ArrowXml a => String -> a XmlTree String
getStat statName = deep (hasName "profile" >>> hasAttrValue "profiletypename" (== "Unit")) >>>
                         deep (
                         hasName "characteristic" >>>
                         hasAttrValue "name" (== statName) >>>
                         getAttrValue "value")

getStats :: ArrowXml a => a XmlTree Stats
getStats = proc el -> do
  move <- getStat "M" -< el
  ws <- getStat "WS" -< el
  bs <- getStat "BS" -< el
  s <- getStat "S" -< el
  t <- getStat "T" -< el
  w <- getStat "W" -< el
  a <- getStat "A" -< el
  ld <- getStat "Ld" -< el
  sa <- getStat "Save" -< el
  returnA -< (Stats move ws bs s t w a ld sa)

getModelGroup :: ArrowXml a => a XmlTree ModelGroup
getModelGroup = proc el -> do
     name <- getAttrValue "name" -< el
     count <- getAttrValue "number" -< el
     stats <- getStats -< el
     returnA -< ModelGroup name (read count) stats

main :: IO ()
main = do
  -- args <- getArgs
  -- let fileToParse = head args
  html <- readFile "test.ros"
  let doc = readString [withParseHTML yes, withWarnings no] html
  units <- runX $ doc >>> findUnits >>> listA (findModels >>> getModelGroup) >>> arr Unit
  print $ length units
  mapM_ print units
  return ()
