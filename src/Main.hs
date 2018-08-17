{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Arrow
import           Control.Lens         hiding (deep, (.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import qualified Data.Text            as T
import           System.Environment
import           Text.XML.HXT.Core
import           TTSJson
import           Types
import Data.Fixed




toDescription :: Stats -> T.Text
toDescription Stats{..} = T.pack $ concat statLines where
  statLines = map (++ "\r\n") rawStatLines
  rawStatLines = [
    "M:  "++ _move,
    "WS: "++ _ws,
    "BS: "++ _bs,
    "S:  "++ _strength,
    "T:  "++ _toughness,
    "W:  "++ _wounds,
    "A:  "++ _attacks,
    "Ld: "++ _leadership,
    "Sa: "++ _save]

findUnits :: ArrowXml a => a XmlTree XmlTree
findUnits = multi (isElem >>> hasName "selection" >>> hasAttrValue "type" (== "unit"))

findNonUnitSelections :: ArrowXml a => a XmlTree XmlTree
findNonUnitSelections = getChildren >>> getChildren >>> getChildren >>> getChildren >>> getChildren >>> 
    hasName "selection"  >>> hasAttrValue "type" (/= "unit")

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
  returnA -< ModelGroup (T.pack name) (read count) stats

modelsPerRow = 10
maxRankXDistance = 22
unitSpacer = 1.2

assignPositionsToModels :: Pos -> Double -> Int -> [Value] -> [Value]
assignPositionsToModels _ _ _ []       = []
assignPositionsToModels basePos@Pos{..} width index (v : vs) = model : remainder where
  newCol = posX + (fromIntegral (index `mod` modelsPerRow) * width)
  newRow = posZ + (fromIntegral (index `quot` modelsPerRow) * width)
  nextPos = Pos newCol posY newRow
  model =  setPos nextPos v
  remainder = assignPositionsToModels basePos width (index + 1) vs

assignPositionsToUnits :: Pos -> [[Value]] -> [[Value]]
assignPositionsToUnits _ [] = []
assignPositionsToUnits pos@Pos{..} (u : us) = models : assignPositionsToUnits nextPos us where
  width =  fromMaybe 1 (u ^?_head. key "Width"._Double)
  models = assignPositionsToModels pos width 0 u
  numModels = length models
  maxXOfUnit = fromIntegral (min numModels modelsPerRow) * width
  nextX = (posX + (maxXOfUnit + unitSpacer)) `mod'` maxRankXDistance
  nextZ = if nextX < posX then posZ + 5.5 else posZ
  nextPos = Pos nextX posY nextZ


retrieveAndModifyUnitJSON :: HM.HashMap T.Text Value -> [Unit] -> [[Maybe Value]]
retrieveAndModifyUnitJSON templateMap units = do
  unit <- units
  let modelGroups = retrieveAndModifyModelGroupJSON templateMap (unit ^. subGroups)
  return modelGroups

retrieveAndModifyModelGroupJSON :: HM.HashMap T.Text Value -> [ModelGroup] -> [Maybe Value]
retrieveAndModifyModelGroupJSON templateMap groups = do
  modelGroup <- groups
  let modelName =  modelGroup ^. name
  let modelBaseJson = HM.lookup modelName templateMap
  let theStats = modelGroup ^. stats
  let description = toDescription theStats
  let nameWithWounds = mconcat $ [modelName, " "] ++
                                if read (theStats ^. wounds) > 1 then
                                  [T.pack (theStats ^. wounds),
                                  "/" ,
                                  T.pack (theStats ^. wounds)]
                                else
                                  []
  let modifiedJson = fmap
                        (setDescription description .
                        setName nameWithWounds)
                          modelBaseJson
  replicate (modelGroup ^. modelCount) modifiedJson

zeroPos :: Pos
zeroPos = Pos 0.0 0.0 0.0

main :: IO ()
main = do
  -- args <- getArgs
  -- let fileToParse = head args
  html <- readFile "test.ros"
  let doc = readString [withParseHTML yes, withWarnings no] html
  units <- runX $ doc >>> findUnits >>> listA (findModels >>> getModelGroup) >>> arr Unit
  nonUnitSelections <- runX $ doc >>> findNonUnitSelections >>> listA getModelGroup >>> arr Unit  
  modelData <- loadModels
  let process = assignPositionsToUnits zeroPos . filter (/= []) . fmap catMaybes . retrieveAndModifyUnitJSON modelData
  let correctedModelJsons = concat $ process (units ++ nonUnitSelections)
  let output = encode (object ["ObjectStates" .= correctedModelJsons])
  B.putStr output
  return ()
