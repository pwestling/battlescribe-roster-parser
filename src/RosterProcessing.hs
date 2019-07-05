{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module RosterProcessing where

import           Control.Arrow
import           Control.Lens               hiding (deep, (.=))
import           Control.Monad
import           Control.Monad.List
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Either
import           Data.Fixed
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Debug.Trace                as Debug
import           Safe
import           System.Environment
import           System.IO
import           Text.XML.HXT.Core
import           TTSJson
import           Types

isType :: ArrowXml a => String -> a XmlTree XmlTree
isType t = hasAttrValue "typename" (== t) <+> hasAttrValue "profiletypename" (== t)

getType :: ArrowXml a => a XmlTree String
getType = getAttrValue "typename" <+> getAttrValue "profiletypename"

getBatScribeValue :: ArrowXml a => a XmlTree String
getBatScribeValue = single ((this /> getText) <+> getAttrValue "value")

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
  allAbilities = _abilities modelGroup ++ Types._unitAbilities unit
  statLines = map (++ "\r\n") rawStatLines
  rawStatLines = [
    statHeader,
    statString stats] ++
    (if not (null allWeapons) then
      textColor "e85545" "Weapons" : map weaponStr allWeapons
    else
      []) ++
    (if not (null allAbilities) then
      textColor "dc61ed" "Abilities"  : allAbilities
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

withForceName :: ArrowXml a => a XmlTree (String -> b) -> a XmlTree b
withForceName arrow =
  deep (isElem >>> hasName "force") >>>
  proc el -> do
    forceName <- getAttrValue "cataloguename" -< el
    result <- arrow -< el
    returnA -< result forceName

findTopLevelUnitsAndModels :: ArrowXml a => a XmlTree XmlTree
findTopLevelUnitsAndModels = deep (isElem >>> hasName "selection") >>> hasAttrValue "type" (\t -> t == "unit" || t == "model")

findNonUnitSelections :: ArrowXml a => a XmlTree XmlTree
findNonUnitSelections = deep ( hasName "selection"  >>> hasAttrValue "type" (\a -> (a /= "unit") && (a /= "model")) >>>
    filterA (deep (isElem >>> hasName "profile" >>> isType "Unit")))

findModels :: ArrowXml a => a XmlTree XmlTree
findModels = multi (isElem >>> hasName "selection" >>> hasAttrValue "type" (== "model"))

getStat :: ArrowXml a => String -> a XmlTree String
getStat statName = this />
                  hasName "characteristic" >>>
                  hasAttrValue "name" (== statName) >>>
                  getBatScribeValue

getWeaponStat :: ArrowXml a => String -> a XmlTree String
getWeaponStat statName = this /> hasName "characteristics" />
                          hasName "characteristic" >>>
                          hasAttrValue "name" (== statName) >>>
                          getBatScribeValue

getAbilities :: ArrowXml a => a XmlTree String
getAbilities = deep (hasName "profile" >>> isType "Abilities" >>> getAttrValue "name")

getAbilitiesShallow :: ArrowXml a => a XmlTree String
getAbilitiesShallow = hasAttrValue "type" (== "unit") >>> getChildren >>> hasName "selections" >>> getChildren >>>
  (hasName "selection" >>> hasAttrValue "type" (== "upgrade")) >>> getAbilities

getStats :: ArrowXml a => a XmlTree Stats
getStats = this //>
           isType "Unit" />
           hasName "characteristics" >>>
              proc el -> do
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
getWeapons = listA $ deep (hasName "profile" >>> isType "Weapon") >>> getWeapon

getWeaponsShallow :: ArrowXml a => a XmlTree [Weapon]
getWeaponsShallow = listA $ hasAttrValue "type" (== "unit") >>> getChildren >>> hasName "selections" >>> getChildren >>>
    (hasName "selection" >>> hasAttrValue "type" (== "upgrade")) >>> getWeapon

getModelGroup :: ArrowXml a => a XmlTree ModelGroup
getModelGroup = proc el -> do
  name <- getAttrValue "name" -< el
  count <- getAttrValue "number"  -< el
  stats <- getStats -< el
  weapons <- getWeapons -< el
  abilities <- listA getAbilities -< el
  returnA -< ModelGroup (T.pack name) (read count) (Just stats) weapons abilities

modelsPerRow = 10
maxRankXDistance = 22
unitSpacer = 1.2

assignPositionsToModels :: Pos -> Double -> [Double] -> [Double] -> Int -> [Value] -> ([Value], Double)
assignPositionsToModels _ _ _ _ _ []       = ([], -100000)
assignPositionsToModels basePos@Pos{..} maxWidth widths usedWidths index (v : vs) = (model : remainder, newWidest) where
  widthsToUse = if index `mod` modelsPerRow == 0 then [] else usedWidths
  newCol = posX + sum widthsToUse
  newRow = posZ + (fromIntegral (index `quot` modelsPerRow) * maxWidth)
  nextPos = Pos newCol posY newRow
  model =  destick (setPos nextPos v)
  (remainder, widest) = assignPositionsToModels basePos maxWidth (tail widths) (head widths : widthsToUse) (index + 1) vs
  newWidest = maximum [widest, newCol + head widths]

assignPositionsToUnits :: Pos -> [[Value]] -> [[Value]]
assignPositionsToUnits _ [] = []
assignPositionsToUnits pos@Pos{..} (u : us) = models : assignPositionsToUnits nextPos us where
  getWidth model = fromMaybe 1 (model ^? key "Width"._Double)
  widths =  fmap getWidth u
  maxWidth = fromMaybe 1 (maximumMay widths)
  (models, maxXOfUnit) = assignPositionsToModels pos maxWidth widths [] 0 u
  numModels = length models
  rawNextX = posX + (maxXOfUnit + unitSpacer)
  nextX =  if rawNextX > maxRankXDistance then 0 else rawNextX
  nextZ = if rawNextX > maxRankXDistance then posZ + 6 else posZ
  nextPos = Pos nextX posY nextZ

retrieveAndModifyUnitJSON :: ModelFinder -> [Unit] -> [[Either String [Value]]]
retrieveAndModifyUnitJSON templateMap units = result where
  retrieve unit = retrieveAndModifyModelGroupJSON templateMap unit (unit ^. subGroups)
  result = map retrieve units

orElseM :: Maybe a -> Maybe a -> Maybe a
orElseM (Just a) _ = Just a
orElseM _ b        = b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Left t)  = Left t
mapRight f (Right t) = Right (f t)

retrieveAndModifySingleGroupJSON :: ModelFinder -> Unit -> ModelGroup -> [Either String [Value]]
retrieveAndModifySingleGroupJSON modelFinder unit modelGroup = result where
   modelName =  modelGroup ^. name
   uName = unit ^. unitName
   modelBaseJson = modelFinder unit modelGroup
   theStats =  fromMaybe (unit ^. unitDefaultStats) (modelGroup ^. stats)
   description = toDescription unit modelGroup theStats
   nameWithWounds = mconcat $ [modelName, " "] ++
                                if read (theStats ^. wounds) > 1 then
                                  [T.pack (theStats ^. wounds),
                                  "/" ,
                                  T.pack (theStats ^. wounds)]
                                else
                                  []
   modifiedJson = fmap
                        (setDescription description .
                        setName nameWithWounds .
                        setScript (T.pack (unit ^. script)))
                          modelBaseJson
   jsonOrErr = maybe (Left (uName ++ " - " ++ T.unpack modelName)) Right modifiedJson
   result = [mapRight (replicate (modelGroup ^. modelCount)) jsonOrErr]

retrieveAndModifyModelGroupJSON :: ModelFinder -> Unit -> [ModelGroup] -> [Either String [Value]]
retrieveAndModifyModelGroupJSON modelFinder unit groups = result where
  results = map (retrieveAndModifySingleGroupJSON modelFinder unit) groups
  result = concat results

zeroPos :: Pos
zeroPos = Pos 0.0 0.0 0.0

addBase :: Value -> [Value] -> [Value]
addBase baseData [] = []
addBase baseData vals = modelsAndBase where
  maxX = fromMaybe 0 $ maximumMay (concatMap (^.. key "Transform" . key "posX"._Double) vals)
  maxZ = fromMaybe 0 $ maximumMay (concatMap (^.. key "Transform" . key "posZ"._Double) vals)
  scaleX = (maxX + 5) / 17.0
  scaleZ = (maxZ + 5) / 17.0
  setTransform trans amount val = val & key "Transform" . key trans._Double .~ amount
  addTransform pos amount val =  val & key "Transform" . key pos._Double %~ (+ amount)
  scaledBase = (setTransform "scaleX" scaleX .
                setTransform "scaleZ" scaleZ) baseData
  respositionedModels = map (addTransform "posX" (maxX / (-2)) .
                             addTransform "posZ" (maxZ / (-2)) .
                             setTransform "posY" 1.5) vals
  modelsAndBase = scaledBase : respositionedModels

da :: (Show s, ArrowXml a) => a s s
da = arr Debug.traceShowId

makeUnit ::  ArrowXml a => a XmlTree ModelGroup -> a XmlTree (String -> Unit)
makeUnit modelFn = proc el -> do
  name <- getAttrValue "name" >>> da -< el
  abilities <- listA getAbilitiesShallow -< el
  weapons <- getWeaponsShallow -< el
  stats <- listA getStats >>> arr listToMaybe >>> arr (fromMaybe zeroStats)  -< el
  models <- listA modelFn -< el
  singleModelUnit <- listA getModelGroup -< el
  let modelsUsed = if null models then singleModelUnit else models
  returnA -< \forceName -> Unit name forceName stats modelsUsed abilities weapons ""

asRoster :: [Value] -> Value
asRoster values = object ["ObjectStates" .= values]

process :: ModelFinder -> Value -> [Unit] -> RosterTranslation
process modelData baseData units = result where
  unitsAndErrors = retrieveAndModifyUnitJSON modelData units
  validUnits = filter (/= []) (fmap (join . rights) unitsAndErrors)
  invalidUnits = filter (/= []) $ (lefts . join) unitsAndErrors
  positioned = assignPositionsToUnits zeroPos validUnits
  unstuck = concatMap (map destick) positioned
  based = addBase baseData unstuck
  roster = asRoster based
  result = RosterTranslation (Just roster) (nub invalidUnits)

zeroStats :: Stats
zeroStats = Stats "" "" "" "" "" "" "" "" ""

createModelDescriptors :: Unit -> [ModelDescriptor]
createModelDescriptors unit = fmap (createModelDescriptor unit) (unit ^. subGroups)

createModelDescriptor :: Unit -> ModelGroup -> ModelDescriptor
createModelDescriptor unit group = ModelDescriptor
                                   (group ^. name)
                                   (T.pack <$> Data.List.sort (fmap _weaponName (group ^. weapons)))

generateRosterNames :: T.Text -> [Unit] -> RosterNamesRequest
generateRosterNames rosterId units = RosterNamesRequest rosterId descriptors where
  descriptors = nub $ concatMap createModelDescriptors units

processRoster :: String -> T.Text -> IO [Unit]
processRoster xml rosterId = do
  let doc = readString [withParseHTML yes, withWarnings no] xml
  units <- runX $ doc >>> withForceName (findTopLevelUnitsAndModels >>> makeUnit (findModels >>> getModelGroup))
  nonUnitSelections <- runX $ doc >>> withForceName (findNonUnitSelections >>> makeUnit getModelGroup)
  let allUnits = units ++ nonUnitSelections
  return allUnits

assignmentToPair :: ModelAssignment -> (ModelDescriptor, Value)
assignmentToPair (ModelAssignment desc val) = (desc, val)

createTTS :: BaseData -> [Unit] -> RosterNamesResponse -> RosterTranslation
createTTS baseData units (RosterNamesResponse assignments) = result where
  map = HM.fromList (fmap assignmentToPair assignments)
  mf unit modelGroup = HM.lookup (createModelDescriptor unit modelGroup) map
  result = process mf baseData units
