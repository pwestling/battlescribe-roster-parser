{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Arrow
import           Control.Lens         hiding (deep, (.=))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Either
import           Data.Fixed
import qualified Data.HashMap.Strict  as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text            as T
import qualified Debug.Trace          as Debug
import           System.Environment
import           System.IO
import           Text.XML.HXT.Core
import           TTSJson
import           TTSUI
import           Types

textColor :: String -> String -> String
textColor c s = "["++c++"]"++s++"[-]"

statHeader :: String
statHeader = textColor "56f442" "M  WS BS  S   T  W   A  LD     SV[-]"

statString :: Stats -> String
statString Stats{..} = fixWidths
  [(_move,3), (_ws,4), (_bs,4),
  (_strength,3), (_toughness,4),(_wounds,4),
  (_attacks,4), (_leadership,4), (_save,8)]

toDescription :: Unit -> ModelGroup -> Stats -> T.Text
toDescription unit modelGroup stats = T.pack $ concat statLines where
  allWeapons = _weapons modelGroup ++ _unitWeapons unit
  statLines = map (++ "\r\n") rawStatLines
  rawStatLines = [
    statHeader,
    statString stats] ++
    (if not (null allWeapons) then
      textColor "e85545" "Weapons" : map weaponStr allWeapons
    else
      []) ++
    (if not (null (_abilities unit)) then
      textColor "dc61ed" "Abilities"  : _abilities unit
    else
      [])

fixWidths :: [(String, Int)] -> String
fixWidths = concatMap f where
  f (str, size) = take size (replicate (max 0 (size - length str)) ' ' ++ str)

weaponFmt :: String -> String -> String -> String -> String -> String -> String
weaponFmt range t str ap d sp  = unwords [if range /= "Melee" then range else "", t, "S:"++str, "AP:"++ap, "D:"++d, "Sp?:"++sp]

weaponHeader :: String
weaponHeader = weaponFmt "Range" "Type" "S" "AP" "D" "Sp"

weaponStr :: Weapon -> String
weaponStr Weapon{..} = textColor "c6c930" _weaponName ++ "\r\n"
  ++ weaponFmt _range _type _weaponStrength _AP _damage (if _special /= "-" then "*" else _special)

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

getWeaponStat :: ArrowXml a => String -> a XmlTree String
getWeaponStat statName = deep (
                          hasName "characteristic" >>>
                          hasAttrValue "name" (== statName) >>>
                          getAttrValue "value")

getAbilities :: ArrowXml a => a XmlTree String
getAbilities = deep (hasName "profile" >>> hasAttrValue "profiletypename" (== "Abilities") >>> getAttrValue "name")

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

getWeapon :: ArrowXml a => a XmlTree Weapon
getWeapon = proc el -> do
  name <- getAttrValue "name" -< el
  range <- getWeaponStat "Range" -< el
  weaponType <- getWeaponStat "Type" -< el
  str <- getWeaponStat "S" -< el
  ap <- getWeaponStat "AP" -< el
  damage <- getWeaponStat "D" -< el
  special <- getWeaponStat "Abilities" -< el
  returnA -< Weapon name range weaponType str ap damage special

getWeapons :: ArrowXml a => a XmlTree [Weapon]
getWeapons = listA $ deep (hasName "profile" >>> hasAttrValue "profiletypename" (== "Weapon")) >>> getWeapon

getWeaponsShallow :: ArrowXml a => a XmlTree [Weapon]
getWeaponsShallow = listA $ hasAttrValue "type" (== "unit") >>> getChildren >>> hasName "selections" >>> getChildren >>>
    (hasName "selection" >>> hasAttrValue "type" (== "upgrade")) >>> getWeapon

getModelGroup :: ArrowXml a => a XmlTree ModelGroup
getModelGroup = proc el -> do
  name <- getAttrValue "name" -< el
  count <- getAttrValue "number" -< el
  stats <- listA getStats -< el
  weapons <- getWeapons -< el
  returnA -< ModelGroup (T.pack name) (read count) (listToMaybe stats) weapons

modelsPerRow = 10
maxRankXDistance = 22
unitSpacer = 1.2

assignPositionsToModels :: Pos -> Double -> Int -> [Value] -> [Value]
assignPositionsToModels _ _ _ []       = []
assignPositionsToModels basePos@Pos{..} width index (v : vs) = model : remainder where
  newCol = posX + (fromIntegral (index `mod` modelsPerRow) * width)
  newRow = posZ + (fromIntegral (index `quot` modelsPerRow) * width)
  nextPos = Pos newCol posY newRow
  model =  destick (setPos nextPos v)
  remainder = assignPositionsToModels basePos width (index + 1) vs

assignPositionsToUnits :: Pos -> [[Value]] -> [[Value]]
assignPositionsToUnits _ [] = []
assignPositionsToUnits pos@Pos{..} (u : us) = models : assignPositionsToUnits nextPos us where
  width =  fromMaybe 1 (u ^?_head. key "Width"._Double)
  models = assignPositionsToModels pos width 0 u
  numModels = length models
  maxXOfUnit = fromIntegral (min numModels modelsPerRow) * width
  rawNextX = posX + (maxXOfUnit + unitSpacer)
  nextX =  rawNextX `mod'` maxRankXDistance
  nextZ = if rawNextX > maxRankXDistance then posZ + 7 else posZ
  nextPos = Pos nextX posY nextZ


retrieveAndModifyUnitJSON :: HM.HashMap T.Text Value -> [Unit] -> [[Either String [Value]]]
retrieveAndModifyUnitJSON templateMap units = do
  unit <- units
  let modelGroups = retrieveAndModifyModelGroupJSON templateMap unit (unit ^. subGroups)
  return modelGroups

orElseM :: Maybe a -> Maybe a -> Maybe a
orElseM (Just a) _ = Just a
orElseM _ b        = b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Left t)  = Left t
mapRight f (Right t) = Right (f t)

retrieveAndModifyModelGroupJSON :: HM.HashMap T.Text Value -> Unit -> [ModelGroup] -> [Either String [Value]]
retrieveAndModifyModelGroupJSON templateMap unit groups = do
  modelGroup <- groups
  let modelName =  modelGroup ^. name
  let uName = unit ^. unitName
  let modelBaseJson =  HM.lookup (T.pack (uName ++ "$" ++ T.unpack modelName)) templateMap
                       `orElseM`
                       HM.lookup modelName templateMap
  let theStats = fromMaybe (unit ^. unitDefaultStats) (modelGroup ^. stats)
  let description = toDescription unit modelGroup theStats
  let nameWithWounds = mconcat $ [modelName, " "] ++
                                if read (theStats ^. wounds) > 1 then
                                  [T.pack (theStats ^. wounds),
                                  "/" ,
                                  T.pack (theStats ^. wounds)]
                                else
                                  []
  let modifiedJson = fmap
                        (setDescription description .
                        setName nameWithWounds .
                        setScript (T.pack (unit ^. script)))
                          modelBaseJson
  let jsonOrErr = maybe (Left ("Could not find unit$model "++uName++"$"++ T.unpack modelName)) Right modifiedJson
  [mapRight (replicate (modelGroup ^. modelCount)) jsonOrErr]

zeroPos :: Pos
zeroPos = Pos 0.0 0.0 0.0

addBase :: HM.HashMap T.Text Value -> [Value] -> [Value]
addBase modelData vals = modelsAndBase where
  maxX = maximum (concatMap (^.. key "Transform" . key "posX"._Double) vals)
  maxZ = maximum (concatMap (^.. key "Transform" . key "posZ"._Double) vals)
  scaleX = (maxX + 5) / 17.0 
  scaleZ = (maxZ + 5) / 17.0 
  base = fromJust (HM.lookup "Base" modelData)
  setTransform trans amount val = val & key "Transform" . key trans._Double .~ amount
  addTransform pos amount val =  val & key "Transform" . key pos._Double %~ (+ amount)
  scaledBase = (setTransform "scaleX" scaleX . 
                setTransform "scaleZ" scaleZ) base
  respositionedModels = map (addTransform "posX" (maxX / (-2)) . 
                             addTransform "posZ" (maxZ / (-2)) .
                             setTransform "posY" 1.5) vals
  modelsAndBase = scaledBase : respositionedModels

makeUnit ::  ArrowXml a => a XmlTree ModelGroup -> a XmlTree Unit
makeUnit modelFn = proc el -> do
  name <- getAttrValue "name" -< el
  abilities <- listA getAbilities -< el
  weapons <- getWeaponsShallow -< el
  stats <- listA getStats >>> arr listToMaybe >>> arr (fromMaybe zeroStats) -< el
  models <- listA modelFn -< el
  script <- (scriptFromXml name) -<< el
  returnA -< Unit name stats models abilities weapons script


process :: HM.HashMap T.Text Value -> [Unit] -> IO [[Value]]
process modelData units = do
  let unitsAndErrors = retrieveAndModifyUnitJSON modelData units
  mapM_ (hPutStrLn stderr) (lefts $ join unitsAndErrors)
  let validUnits = filter (/= []) (fmap (join . rights) unitsAndErrors)
  let positioned = assignPositionsToUnits zeroPos validUnits
  let unstuck = map (map destick) positioned
  return unstuck

zeroStats :: Stats
zeroStats = undefined

main :: IO ()
main = do
  -- args <- getArgs
  -- let fileToParse = head args
  html <- getContents
  let doc = readString [withParseHTML yes, withWarnings no] html
  units <- runX $ doc >>> findUnits >>> makeUnit (findModels >>> getModelGroup)
  nonUnitSelections <- runX $ doc >>> findNonUnitSelections >>> makeUnit getModelGroup
  --mapM_ (hPrint stderr) units
  modelData <- loadModels
  correctedModelJsons <- concat <$> process modelData (units ++ nonUnitSelections)
  let basedModelJsons = addBase modelData correctedModelJsons
  let output = encode (object ["ObjectStates" .= basedModelJsons])
  B.putStr output
  return ()
