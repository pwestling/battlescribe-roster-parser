{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

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
import           Database.Redis
import           Databases
import qualified Debug.Trace                as Debug
import           Safe
import           Server
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
  stats <- listA getStats -< el
  weapons <- getWeapons -< el
  abilities <- listA getAbilities -< el
  returnA -< ModelGroup (T.pack name) (read count) (listToMaybe stats) weapons abilities

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

retrieveAndModifyUnitJSON :: ModelFinder -> User -> [Unit] -> IO [[Either String [Value]]]
retrieveAndModifyUnitJSON templateMap user units = do
  let retrieve unit = retrieveAndModifyModelGroupJSON templateMap user unit (unit ^. subGroups)
  mapM retrieve units

orElseM :: Maybe a -> Maybe a -> Maybe a
orElseM (Just a) _ = Just a
orElseM _ b        = b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Left t)  = Left t
mapRight f (Right t) = Right (f t)

retrieveAndModifySingleGroupJSON :: ModelFinder -> User -> Unit -> ModelGroup -> IO [Either String [Value]]
retrieveAndModifySingleGroupJSON modelFinder user unit modelGroup = do
  let modelName =  modelGroup ^. name
  let uName = unit ^. unitName
  modelBaseJson <- modelFinder user unit modelGroup
  let theStats =  fromMaybe (unit ^. unitDefaultStats) (modelGroup ^. stats)
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
  let jsonOrErr = maybe (Left (uName ++ " - " ++ T.unpack modelName)) Right modifiedJson
  return [mapRight (replicate (modelGroup ^. modelCount)) jsonOrErr]

retrieveAndModifyModelGroupJSON :: ModelFinder -> User -> Unit -> [ModelGroup] -> IO [Either String [Value]]
retrieveAndModifyModelGroupJSON modelFinder user unit groups = do
  results <- mapM (retrieveAndModifySingleGroupJSON modelFinder user unit) groups
  return $ concat results

zeroPos :: Pos
zeroPos = Pos 0.0 0.0 0.0

addBase :: Value -> [Value] -> [Value]
addBase baseData [] = []
addBase baseData vals = modelsAndBase where
  maxX = maximum (concatMap (^.. key "Transform" . key "posX"._Double) vals)
  maxZ = maximum (concatMap (^.. key "Transform" . key "posZ"._Double) vals)
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
  script <- (scriptFromXml name) -<< el
  let modelsUsed = if null models then singleModelUnit else models
  returnA -< \forceName -> Unit name forceName stats modelsUsed abilities weapons script

asRoster :: [Value] -> Value
asRoster values = object ["ObjectStates" .= values]

process :: ModelFinder -> Value -> User -> [Unit] -> IO RosterTranslation
process modelData baseData user units = do
  unitsAndErrors <- retrieveAndModifyUnitJSON modelData user units
  let validUnits = filter (/= []) (fmap (join . rights) unitsAndErrors)
  let invalidUnits = filter (/= []) $ (lefts . join) unitsAndErrors
  mapM_ (hPutStrLn stderr) invalidUnits
  let positioned = assignPositionsToUnits zeroPos validUnits
  let unstuck = concatMap (map destick) positioned
  let based = addBase baseData unstuck
  let roster = asRoster based
  return $ RosterTranslation (Just roster) (nub invalidUnits)

zeroStats :: Stats
zeroStats = Stats "" "" "" "" "" "" "" "" ""

redisModelFinder :: Connection -> ModelFinder
redisModelFinder conn user unit modelGroup = do
  let lookupKeys = encodeUtf8 <$> generateLookupKeys user unit modelGroup
  runRedis conn $ do
    found <- mapM get lookupKeys
    let successes = rights found
    let value = headMay (catMaybes successes)
    return $ join $ fmap (decode' . C8.fromStrict) value

redisModelAdder :: Connection -> T.Text -> Value -> IO ()
redisModelAdder conn key modelData = runRedis conn $ do
  setnx (encodeUtf8 key) (C8.toStrict (encode modelData))
  return ()

-- commandLine :: IO ()
-- commandLine = do
--   html <- getContents
--   modelData <- loadModels
--   let baseData = fromJust $ HM.lookup "base" modelData
--   output <- processRoster (staticMapModelFinder modelData) baseData (User "") html
--   --putStr output
--   return ()

processRoster :: BaseData -> ModelFinder -> User -> String -> IO RosterTranslation
processRoster  baseData modelFinder  user rosterXML = do
  let doc = readString [withParseHTML yes, withWarnings no] rosterXML
  units <- runX $ doc >>> withForceName (findTopLevelUnitsAndModels >>> makeUnit (findModels >>> getModelGroup))
  nonUnitSelections <- runX $ doc >>> withForceName (findNonUnitSelections >>> makeUnit getModelGroup)
  --mapM_ (hPrint stderr) units
  process modelFinder baseData user (units ++ nonUnitSelections)

merge :: ModelFinder -> ModelFinder -> ModelFinder
merge mf1 mf2 user unit modelGroup = do
  value1 <- mf1 user unit modelGroup
  if isJust value1 then return value1 else mf2 user unit modelGroup

server :: IO ()
server = do
  modelData <- loadModels
  conn <- connect defaultConnectInfo
  let baseData = fromJust $ HM.lookup "base" modelData
  let modelFinder = merge (redisModelFinder conn) (staticMapModelFinder modelData)
  startApp (processRoster  baseData ) (redisModelAdder conn) 8080

main :: IO ()
main = server
