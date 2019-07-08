{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}


module RosterProcessing where

import           Control.Arrow
import           Control.Arrow.ListArrow
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
isType t = hasAttrValue "typename" (== t) <+> hasAttrValue "profiletypename" (== t) <+> hasAttrValue "type" (== t)

isTypeF :: ArrowXml a => (String -> Bool) -> a XmlTree XmlTree
isTypeF f = hasAttrValue "typename" f <+> hasAttrValue "profiletypename" f <+> hasAttrValue "type" f

getType :: ArrowXml a => a XmlTree String
getType = getAttrValue "typename" <+> getAttrValue "profiletypename" <+> getAttrValue "type"

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
  allWeapons = nub (_weapons modelGroup ++ _unitWeapons unit)
  allAbilities = nub (_abilities modelGroup ++ Types._unitAbilities unit)
  statLines = map (++ "\r\n") rawStatLines
  rawStatLines = [
    statHeader,
    statString stats] ++
    (if not (null allWeapons) then
      textColor "e85545" "Weapons" : map weaponStr (dedupeWeapons allWeapons)
    else
      []) ++
    (if not (null allAbilities) then
      textColor "dc61ed" "Abilities"  : map abilityString allAbilities
    else
      [])

fixWidths :: [(String, Int)] -> String
fixWidths = concatMap f where
  f (str, size) = take size (replicate (max 0 (size - length str)) ' ' ++ str)

weaponFmt :: String -> String -> String -> String -> String -> String -> String
weaponFmt range t str ap d sp  = unwords [if range /= "Melee" then range else "", t, "S:"++str, "AP:"++ap, "D:"++d, "Sp?:"++sp]

weaponHeader :: String
weaponHeader = weaponFmt "Range" "Type" "S" "AP" "D" "Sp"

countString :: Int -> String
countString count = if count > 1 then show count ++ "x " else ""

weaponById :: Weapon -> (Weapon, Weapon)
weaponById w = (w { _id = ""}, w)

dedupeWeapons :: [Weapon] -> [Weapon]
dedupeWeapons weapons = HM.elems grouped where
  merge w1 w2 = w1 {_count = _count w1 +  _count w2}
  grouped = HM.fromListWith merge (fmap weaponById weapons)

weaponStr :: Weapon -> String
weaponStr Weapon{..} = textColor "c6c930" (countString _count ++  _weaponName ++ "\r\n")
  ++ weaponFmt _range _type _weaponStrength _AP _damage (if _special /= "-" then "*" else _special)

abilityString :: Ability -> String
abilityString Ability{..} = _abilityName

withForceName :: ArrowXml a => a XmlTree (String -> b) -> a XmlTree b
withForceName arrow =
  deep (isElem >>> hasName "force") >>>
  proc el -> do
    forceName <- getAttrValue "cataloguename" -< el
    result <- arrow -< el
    returnA -< result forceName

findSelectionsRepresentingModels :: ArrowXml a => a XmlTree XmlTree
findSelectionsRepresentingModels = deep (hasName "selection") >>>
    filterA (deep (isElem >>> hasName "profile" >>> isType "Unit")) >>> da "Found Unit: "

findModels :: ArrowXml a => a XmlTree XmlTree
findModels = multi (isElem >>> hasName "selection" >>> filterA (this /> hasName "profiles" /> hasName "profile" >>> isType "Unit" ))

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

getAbilityDescription :: ArrowXml a => a XmlTree (Maybe String)
getAbilityDescription = listA (this /> hasName "characteristics" />
                                hasName "characteristic" >>> hasAttrValue "name" (== "Description") >>>
                                getBatScribeValue) >>> arr listToMaybe

getAbilities :: ArrowXml a => a XmlTree [Ability]
getAbilities = listA $ profileOfThisModel "Abilities" >>>
    proc el -> do
    name <- getAttrValue "name" -< el
    id <- getAttrValue "id" -< el
    desc <- getAbilityDescription -< el
    returnA -< (Ability name id (fromMaybe "" desc))


getStats :: ArrowXml a => a XmlTree Stats
getStats = profileOfThisModel "Unit" />
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

getWeapon :: ArrowXml a => Int -> a (PartialWeapon, XmlTree) Weapon
getWeapon modelCount = proc (partial, el) -> do
  range <- getWeaponStat "Range" -< el
  weaponType <- getWeaponStat "Type" -< el
  str <- getWeaponStat "S" -< el
  ap <- getWeaponStat "AP" -< el
  damage <- getWeaponStat "D" -< el
  special <- getWeaponStat "Abilities" -< el
  returnA -< Weapon (_partialWeaponName partial) range weaponType str ap damage special (_partialWeaponCount partial) (_partialWeaponId partial)

profileOfThisModel :: ArrowXml a => String -> a XmlTree XmlTree
profileOfThisModel profileType = this />
                    ((hasName "selections" /> hasName "selection" >>> isType "upgrade" /> hasName "profiles" /> hasName "profile" >>> isType profileType)
                    <+> (hasName "profiles" /> hasName "profile" >>> isType profileType))

profileOfThisModelWithSelectionData :: ArrowXml a => String -> a XmlTree b -> a XmlTree (b, XmlTree)
profileOfThisModelWithSelectionData profileType selectionFn = this />
                    (hasName "selections" /> hasName "selection" >>> isType "upgrade" >>> (selectionFn &&& (this /> hasName "profiles" /> hasName "profile" >>> isType profileType)))

data PartialWeapon = PartialWeapon {_partialWeaponId :: String, _partialWeaponName :: String, _partialWeaponCount :: Int}

weaponPartial :: ArrowXml a => Int -> a XmlTree PartialWeapon
weaponPartial modelCount = proc el -> do
  name <- getAttrValue "name" -< el
  id <- getAttrValue "id" -< el
  count <- getAttrValue "number" >>> arr (maybe (-1) (`quot` modelCount) . readMay) -< el
  returnA -< PartialWeapon id name count


getWeapons :: ArrowXml a => Int -> a XmlTree [Weapon]
getWeapons modelCount = listA $ profileOfThisModelWithSelectionData "Weapon" (weaponPartial modelCount) >>> getWeapon modelCount

getModelGroup :: ArrowXml a => a XmlTree ModelGroup
getModelGroup = proc el -> do
  name <- getAttrValue "name" >>> da "Name: " -< el
  count <- getAttrValue "number" >>> da "Count: " >>> arr read -< el
  stats <- getStats >>> da "Stats: " -< el
  weapons <- getWeapons count >>> da "Weapons: " -<< el
  abilities <- getAbilities >>> da "Abilities: " -< el
  returnA -< ModelGroup (T.pack name) count (Just stats) weapons abilities

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

da :: (Show s, ArrowXml a) => String -> a s s
da header = arr (\o -> Debug.trace (header ++ show o) o)

makeUnit ::  ArrowXml a => a XmlTree ModelGroup -> a XmlTree (String -> Unit)
makeUnit modelFn = proc el -> do
  name <- getAttrValue "name" >>> da "Unit: " -< el
  abilities <- getAbilities -< el
  weapons <- getWeapons 1 -< el
  stats <- listA getStats >>> arr listToMaybe >>> arr (fromMaybe zeroStats)  -< el
  models <- listA modelFn -< el
  returnA -< \forceName -> Unit name forceName stats models abilities weapons ""

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
  runX $ doc >>> withForceName (findSelectionsRepresentingModels >>> makeUnit
           (findModels >>> da "Found Model: " >>> getModelGroup >>> da "Made Group: ")) >>> da "Made Unit: "

assignmentToPair :: ModelAssignment -> (ModelDescriptor, Value)
assignmentToPair (ModelAssignment desc val) = (desc, val)

createTTS :: BaseData -> [Unit] -> RosterNamesResponse -> RosterTranslation
createTTS baseData units (RosterNamesResponse assignments) = result where
  map = HM.fromList (fmap assignmentToPair assignments)
  mf unit modelGroup = HM.lookup (createModelDescriptor unit modelGroup) map
  result = process mf baseData units
