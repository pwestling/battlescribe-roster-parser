{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE QuantifiedConstraints #-}


module RosterProcessing where

import           Control.Arrow
import           Control.Arrow.ListArrow
import           Control.Lens               hiding (deep, (.=))
import           Control.Monad
import           Control.Monad.List
import           Control.Monad.Random
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Char
import           Data.Either
import           Data.Fixed
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Vector                as Vec
import qualified Debug.Trace                as Debug
import           Safe
import           System.Environment
import           System.IO
import           Text.XML.HXT.Core
import           TTSJson
import           TTSUI
import           Types
import           XmlHelper
import           ModelScript

{-# ANN module ("HLint: ignore Use =<<" :: String) #-}

chain :: [a -> a] -> a -> a
chain = foldl' (.) id

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
  allWeapons = nub (_weapons modelGroup)
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
    forceName <- getAttrValue "cataloguename" >>> da "Force: " -< el
    result <- arrow -< el
    returnA -< result forceName

findSelectionsRepresentingModels :: ArrowXml a => a XmlTree XmlTree
findSelectionsRepresentingModels = deep (hasName "selection") -- >>>
    -- filterA (deep (isElem >>> hasName "profile" >>> isType "Unit"))

hasUnitProfile :: ArrowXml a => a XmlTree XmlTree
hasUnitProfile = this /> hasName "profiles" /> hasName "profile" >>> isType "Unit"

hasWeaponSelection ::  ArrowXml a => a XmlTree XmlTree
hasWeaponSelection = this /> hasName "selections" /> hasName "selection" /> hasName "profiles" /> hasName "profile" >>> isType "Weapon"

-- isModelOrHasUnit :: ArrowXml a => String -> a XmlTree XmlTree
-- isModelOrHasUnit topId = isType "model"
--                        <+>  (this >>> hasAttrValue "id" (/= topId) /> hasName "profiles" /> hasName "profile" >>> isType "Unit")
--                        <+>

printNameAndId :: ArrowXml a => String -> a XmlTree XmlTree
printNameAndId header = (this &&& getNameAttrValue &&& getAttrValue "id")
                        >>> arr (\(v,(n,i)) -> Debug.trace (header ++ "{ Name => " ++ n ++", Id => " ++ i ++ "}") v)

isModelOrUnit :: ArrowXml a => a XmlTree XmlTree
isModelOrUnit = isType "Unit" <+> isType "model"

containsNoModelsOrUnits :: ArrowXml a => a XmlTree XmlTree
containsNoModelsOrUnits = neg $ deep (isType "Unit" <+> isType "model")

deepWithout :: (Tree t, ArrowXml a) => a (t b) (t b) -> a (t b) (t b) -> a (t b) (t b)
deepWithout guard predicate = deep (guard <+> predicate) >>> filterA (neg guard)

isSpecialCaseSelection :: ArrowXml a => a XmlTree XmlTree
isSpecialCaseSelection = deep $ isElem >>> hasName "selection" >>> hasAttrValue "name" (`elem` exceptions) where
  exceptions = [
    "Ravenwing Talonmaster",
    "Plague Champion",
    "Plague Marine w/ melee weapons",
    "Plague Marine w/ boltgun",
    "Plague Marine w/ Special Weapon"
    ]

isSpecialCaseSubGroup :: ArrowXml a => ScriptOptions -> a XmlTree XmlTree
isSpecialCaseSubGroup options = multi $ isElem >>> hasName "selection" >>> (hasAttrValue "name" (`elem` exceptions) `orElse` hasAttrValue "entryid" (`elem` exceptionIds)) where
  exceptions = [
    "Space Marine w/Special Weapon",
    "Grot Oiler",
    "Big Mek W/ Shokk Attack Gun",
    "Big Mek W/ Kustom Force Field",
    "Big Mek in Mega Armour",
    "Big Mek [Legends]",
    "Grot Orderly (Index)",
    "Painboy",
    "Techmarine Gunner"  
    ] ++ maybe [] (map T.unpack . T.splitOn "\n" . T.pack) (modelNames options)
  exceptionIds = [
    "e050-9739-5f42-0094::f1a3-48e8-0804-fb8e" -- Thunderfire Cannon
    ]

findModels :: ArrowXml a => ScriptOptions -> String -> a XmlTree XmlTree
findModels options topId =
      listA (
      isSpecialCaseSelection `orElse`

      (multi (isSelection >>> filterA (isType "model"))
      <+> multi (isSelection >>> isNotTop >>> filterA hasUnitProfile)
      <+> isSpecialCaseSubGroup options
      -- <+>  deepWithout isModelOrUnit (isSelection >>> isNotTop >>> filterA containsNoModelsOrUnits >>> filterA hasWeaponSelection)
      ) `orElse`

      deep (isSelection >>> inheritsSomeProfile (isSelection >>> hasWeaponsAndIsntInsideModel)) `orElse`

      multi (isSelection >>> filterA hasUnitProfile)) >>>
      arr nub >>> unlistA >>> printNameAndId "Models: " where
        isSelection = isElem >>> hasName "selection"
        isNotTop = hasAttrValue "id" (/= topId)
        hasWeaponsAndIsntInsideModel = isType "model" `orElse` hasWeaponSelection
        inheritsSomeProfile ar = filterA hasUnitProfile >>> deep (filterA ar)

getStat :: ArrowXml a => String -> a XmlTree String
getStat statName = this />
                  hasName "characteristic" >>>
                  hasNameAttrValue (== statName) >>>
                  getBatScribeValue

getStatF :: ArrowXml a => (String -> Bool) -> a XmlTree String
getStatF pred = this />
                  hasName "characteristic" >>>
                  hasNameAttrValue pred >>>
                  getBatScribeValue

getWeaponStat :: ArrowXml a => String -> a XmlTree String
getWeaponStat statName = this /> hasName "characteristics" />
                          hasName "characteristic" >>>
                          hasNameAttrValue (== statName) >>>
                          getBatScribeValue

getAbilityDescription :: ArrowXml a => a XmlTree (Maybe String)
getAbilityDescription = listA ((this /> hasName "characteristics" />
                                hasName "characteristic" >>> hasNameAttrValue (== "Description") >>>
                                getBatScribeValue) <+> (this /> hasName "description" /> getText )) >>> arr listToMaybe

getAbilities :: ArrowXml a => a XmlTree [Ability]
getAbilities = listA $ (profileOfThisModel "Abilities" <+> ruleOfThisModel) >>>
    proc el -> do
    name <- getNameAttrValue -< el
    id <- getAttrValue "id" -< el
    desc <- getAbilityDescription -< el
    returnA -< (Ability name id (fromMaybe "" desc))

hasNoProfiles :: ArrowXml a => a XmlTree XmlTree
hasNoProfiles = neg $ deep ( hasName "profile" )

getUpgrades :: ArrowXml a => a XmlTree [Upgrade]
getUpgrades = listA $ this /> hasName "selections" /> hasName "selection" >>>  isType "upgrade"  >>> filterA hasNoProfiles >>>
    proc el -> do
    name <- getNameAttrValue -< el
    id <- getAttrValue "id" -< el
    returnA -< (Upgrade name id)

getStats :: ArrowXml a => a XmlTree (String, Stats)
getStats = (profileOfThisModel "Unit"  `orElse` profileOfThisModel "Model") >>>
           (getNameAttrValue &&& (this /> hasName "characteristics" >>>
              proc el -> do
              move <- getStat "M" -< el
              ws <- getStat "WS" -< el
              bs <- getStat "BS" -< el
              s <- getStat "S" -< el
              t <- getStat "T" -< el
              w <- getStat "W" -< el
              a <- getStat "A" -< el
              ld <- getStat "Ld" -< el
              sa <- getStatF (`elem` ["Save", "Sv"]) -< el
              returnA -< (Stats move ws bs s t w a ld sa)))

getWeapon :: ArrowXml a => Int -> a (PartialWeapon, XmlTree) Weapon
getWeapon modelCount = proc (partial, el) -> do
  range <- getWeaponStat "Range" -< el
  weaponType <- getWeaponStat "Type" -< el
  str <- getWeaponStat "S" -< el
  ap <- getWeaponStat "AP" -< el
  damage <- getWeaponStat "D" -< el
  special <- getWeaponStat "Abilities" -< el
  name <- getNameAttrValue `orElse` arr (const (_partialWeaponName partial)) -<< el
  returnA -< Weapon name range weaponType str ap damage special (_partialWeaponCount partial) (_partialWeaponId partial)

ruleOfThisModel :: ArrowXml a => a XmlTree XmlTree
ruleOfThisModel = this /> hasName "rules" /> hasName "rule"

profileOfThisModel :: ArrowXml a => String -> a XmlTree XmlTree
profileOfThisModel profileType = this />
                    ((hasName "selections" /> hasName "selection" >>> isType "upgrade" /> hasName "profiles" /> hasName "profile" >>> isType profileType)
                    <+> (hasName "profiles" /> hasName "profile" >>> isType profileType))

profileOfThisModelWithSelectionDataDeep :: ArrowXml a => String -> a XmlTree b -> a XmlTree (b, XmlTree)
profileOfThisModelWithSelectionDataDeep profileType selectionFn = this />
                    (hasName "selections" /> hasName "selection" >>> isType "upgrade" >>>
                    (profileOfThisModelWithSelectionDataDeep profileType selectionFn <+>
                    (selectionFn &&& (this /> hasName "profiles" /> hasName "profile" >>> isType profileType))))


profileOfThisModelWithSelectionData :: ArrowXml a => String -> a XmlTree b -> a XmlTree (b, XmlTree)
profileOfThisModelWithSelectionData profileType selectionFn = profileOfThisModelWithSelectionDataDeep profileType selectionFn `orElse` 
                    (this >>> (selectionFn &&& (this /> hasName "profiles" /> hasName "profile" >>> isType profileType)))

data PartialWeapon = PartialWeapon {_partialWeaponId :: String, _partialWeaponName :: String, _partialWeaponCount :: Int}

weaponPartial :: ArrowXml a => Int -> a XmlTree PartialWeapon
weaponPartial modelCount = proc el -> do
  name <- getNameAttrValue -< el
  id <- getAttrValue "id" -< el
  count <- getAttrValue "number" >>> arr (maybe (-1) (`quot` modelCount) . readMay) -< el
  returnA -< PartialWeapon id name count


getWeapons :: ArrowXml a => Int -> a XmlTree [Weapon]
getWeapons modelCount = listA $ profileOfThisModelWithSelectionData "Weapon" (weaponPartial modelCount) >>> getWeapon modelCount

getWeaponsShallow :: ArrowXml a => [String] -> Int -> a XmlTree [Weapon]
getWeaponsShallow foundGroups modelCount = listA $ this /> deepWithout blockFoundGroups (hasName "selection" >>> filterA anyWeaponSelection) >>> (weaponPartial modelCount &&& anyWeaponSelection) >>> getWeapon modelCount where
  blockFoundGroups = hasAttrValue "id" (`elem` foundGroups)
  anyWeaponSelection = hasName "selection" /> hasName "profiles" /> hasName "profile" >>> isType "Weapon"

getNameAndMultiplier :: String -> (String, Int)
getNameAndMultiplier name = result where
  (digits, rest) = span isDigit name
  result = if not (null digits) && "x " `isPrefixOf` rest then (drop 2 rest, read digits) else (name, 1)


filterByA :: (ArrowXml a, Show b) => (b -> Bool) -> a b b
filterByA pred = this >>. filter pred

getModelGroup :: ArrowXml a => ScriptOptions -> [(String, Stats)] -> a XmlTree ModelGroup
getModelGroup options defaultStats = proc el -> do
  (name, mult) <- getNameAttrValue >>> da "Model Group: " >>> arr getNameAndMultiplier -< el
  id <- getAttrValue "id" -< el
  defaultFirstStat <- single (arr (const defaultStats) >>> unlistA >>^ snd) `withDefault` zeroStats -< el
  defaultMatchingStat <- (single (arr (const defaultStats) >>> da "Defaults: " >>> unlistA >>> filterByA (\p -> Debug.traceShowId (fst p == Debug.traceShowId name))) >>^ snd) `withDefault` defaultFirstStat -<< el
  count <- getAttrValue "number" >>> arr readMay >>> arr (fromMaybe 0) >>> arr (* mult)  >>> da "Model Count: " -<< el
  stats <- listA (((getStats >>> da "Stats: ") `orElse` arr (const ("",defaultMatchingStat))) >>> arr snd)  -<< el
  weapons <- getWeapons count >>> arr (excludeWeapons options) >>> da "Weapons: "  -<< el
  abilities <- getAbilities >>> arr (removeAbilities options) -< el
  upgrades <- getUpgrades  >>> da "Upgrades: "-< el
  returnA -< ModelGroup id (T.pack name) count (listToMaybe stats) weapons abilities upgrades

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

retrieveAndModifyUnitJSON :: T.Text -> ModelFinder -> [Unit] -> IO [[Either String [Value]]]
retrieveAndModifyUnitJSON rosterId templateMap units = result where
  retrieve unit = retrieveAndModifyModelGroupJSON rosterId templateMap unit (unit ^. subGroups)
  result = mapM retrieve units

orElseM :: Maybe a -> Maybe a -> Maybe a
orElseM (Just a) _ = Just a
orElseM _ b        = b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Left t)  = Left t
mapRight f (Right t) = Right (f t)

modelSet ::  ModelFinder -> Unit -> ModelGroup -> T.Text -> T.Text -> T.Text -> IO (Maybe Value)
modelSet finder unit modelGroup nameWithWounds description rosterId = do
    maybeJson <- finder unit modelGroup
    case maybeJson of
      Just json ->  do
                      let modifiedJson = (setDescription description . setName nameWithWounds) json
                      let childScript = setScript (descriptionScript rosterId (T.pack (unit ^. unitSelectionId))) modifiedJson
                      return (Just childScript)
      Nothing -> pure Nothing

retrieveAndModifySingleGroupJSON :: T.Text -> ModelFinder -> Unit -> ModelGroup -> IO [Either String [Value]]
retrieveAndModifySingleGroupJSON rosterId modelFinder unit modelGroup = do
   let modelName =  modelGroup ^. name
   let uName = unit ^. unitName
   let theStats =  fromMaybe ((snd . head) (unit ^. unitDefaultStats)) (modelGroup ^. stats)
   let description = toDescription unit modelGroup theStats
   let woundCount = fromMaybe 0 (readMay (theStats ^. wounds))
   let nameWithWounds = mconcat $ (if woundCount > 1 then
                                    [T.pack (theStats ^. wounds),
                                    "/" ,
                                    T.pack (theStats ^. wounds),
                                    " "]
                                  else
                                    []) ++ [modelName]
   let nonScriptedModelCount = (modelGroup ^. modelCount) - 1
   modelSet <- replicateM (modelGroup ^. modelCount) (modelSet modelFinder unit modelGroup nameWithWounds description rosterId)
   return [maybe (Left (uName ++ " - " ++ T.unpack modelName)) Right (sequence modelSet)]

changeFirstWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
changeFirstWhere pred fn [] = []
changeFirstWhere pred fn (a: as) = if pred a then fn a : as else a : changeFirstWhere pred fn as

hasValue :: Either String [Value] -> Bool
hasValue (Right (v:vs) ) = True
hasValue _               = False

setMasterScript :: Unit -> Either String [Value] -> Either String [Value]
setMasterScript unit (Right (v : vs)) = Right (setScript (T.pack (unit ^. script)) v : vs)
setMasterScript unit _ = error "Predicate should have prevent there being no valid values"

retrieveAndModifyModelGroupJSON :: T.Text -> ModelFinder -> Unit -> [ModelGroup] -> IO [Either String [Value]]
retrieveAndModifyModelGroupJSON rosterId modelFinder unit groups = do
  results <- mapM (retrieveAndModifySingleGroupJSON rosterId modelFinder unit) groups
  return $ changeFirstWhere hasValue (setMasterScript unit) (concat results)


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

multiCompare :: Ord b => [a -> b] -> a -> a -> Ordering
multiCompare (comp : comparators) a1 a2 = case compare (comp a1) (comp a2) of
  EQ -> multiCompare comparators a1 a2
  LT -> LT
  GT -> GT 
multiCompare [] a1 a2 = EQ

addUnitWeapons :: [Weapon] -> [ModelGroup] -> [ModelGroup]
addUnitWeapons [] g = g
addUnitWeapons w g  = chain addWeapons (sort g) where
  comparator = multiCompare [_modelCount, negate . extractStat _leadership, negate . extractStat _attacks, negate . length . _weapons]
  sort = sortBy (flip comparator)
  addWeapons =  map ((. sort) . addUnitWeapon) w

copiableWeapons :: [String]
copiableWeapons = ["Stalker Bolt Rifle",
                   "Bolt rifle",
                   "Auto Bolt Rifle",
                   "Plasma incinerator, Standard",
                   "Plasma incinerator, Supercharge",
                   "Heavy Plasma Incinerator, Standard",
                   "Heavy Plasma Incinerator, Supercharged",
                   "Assault Plasma Incinerator, Standard",
                   "Assault Plasma Incinerator, Supercharged",
                   "Fragstorm Grenade Launcher",
                   "Auto Boltstorm Gauntlets (Shooting)",
                   "Auto Boltstorm Gauntlets (Melee)"]

weaponShouldBeCopied :: Weapon -> Bool
weaponShouldBeCopied w = _weaponName w `elem` copiableWeapons

isWargear :: Ability -> Bool
isWargear ability = any (hasPrefix (_abilityName ability)) prefixes where
  prefixes = ["Icon", "Instrument", "Daemonic Icon"]
  hasPrefix = flip isPrefixOf

addUnitWeapon :: Weapon -> [ModelGroup] -> [ModelGroup]
addUnitWeapon w (g : groups)
  | wepC == 1 && weaponShouldBeCopied w = Debug.trace ("Single wep special case " ++ _weaponName w) $ g {_weapons = w{ _count = 1} : _weapons g, _modelCount = modelC } : addUnitWeapon w groups
  | wepC < modelC = Debug.trace ("Fewer weps than models " ++ _weaponName w) [g {_weapons = w{ _count = 1} : _weapons g, _modelCount = wepC }, g{_modelCount = remModels}] ++ groups
  | wepC `mod` modelC == 0 = Debug.trace ("Divisble weps per model " ++ _weaponName w) $ g {_weapons = w{_count = wepsPerModel} : _weapons g} : groups
  | wepC > modelC = Debug.trace ("More weps than models " ++ _weaponName w) $ addUnitWeapon w{ _count = modelC} [g] ++ addUnitWeapon w{ _count = wepC - modelC} groups where
    wepC = _count w
    wepType = _type w
    modelC = _modelCount g
    remModels = modelC - wepC
    wepsPerModel = wepC `quot` modelC
addUnitWeapon w [] = []


extractStat :: (Stats -> String) -> ModelGroup -> Int
extractStat getter = fromMaybe 0 . join . fmap (readMay . getter) . _stats

addWargear :: [Ability] -> [ModelGroup] -> [ModelGroup]
addWargear abilities groups = chain addWargears (sort groups) where
    wargear = filter isWargear abilities
    comparator = multiCompare [_modelCount, negate . extractStat _leadership, negate . extractStat _attacks, negate . length . _abilities]
    sort = sortBy (flip comparator)
    addWargears =  map ((. sort) . addSingleWargear) wargear

addSingleWargear :: Ability -> [ModelGroup] -> [ModelGroup]
addSingleWargear ability (g : gs) = g {_modelCount = 1, _abilities = ability : _abilities g} : g {_modelCount = _modelCount g - 1} : gs
addSingleWargear _ gs = gs

removeGrenades :: ScriptOptions -> [Weapon] -> [Weapon]
removeGrenades (ScriptOptions _ _ _ excludeGrenades _ _ _) weapons = if excludeGrenades then filter (not . isGrenade) weapons else weapons where
  isGrenade weapon = "Grenade" `isInfixOf`_type weapon

removeSidearmPistols :: ScriptOptions -> [Weapon] -> [Weapon]
removeSidearmPistols (ScriptOptions _ _ _ _ excludeSidearms _ _) weapons = if excludeSidearms then filter (not . isSidearm) weapons else weapons where
  isWeak weapon = maybe False (< 5) (readMay (_weaponStrength weapon))
  isPistol weapon = "Pistol" `isInfixOf` _type weapon
  nonPistolRanged weapon = not (isPistol weapon) && _range weapon /= "Melee"
  hasNonPistolRanged = any nonPistolRanged weapons
  isSidearm weapon = hasNonPistolRanged && isWeak weapon && isPistol weapon

excludeWeapons ::  ScriptOptions -> [Weapon] -> [Weapon]
excludeWeapons options = removeGrenades options . removeSidearmPistols options

removeAbilities :: ScriptOptions -> [Ability] -> [Ability]
removeAbilities (ScriptOptions _ _ _ _ _ excludeAbilities _) abilities = if excludeAbilities then [] else abilities


makeUnit ::  ArrowXml a => ScriptOptions -> T.Text -> a XmlTree (String -> Unit)
makeUnit options rosterId = proc el -> do
  name <- getNameAttrValue >>> da "Unit Name: " -< el
  selectionId <- getAttrValue "id" -< el
  abilities <- getAbilities >>> arr (removeAbilities options) -< el
  stats <- listA (getStats `orElse` arr (const ("None", zeroStats)))  -< el
  models <- listA (findModels options selectionId) -<< el
  modelGroups <- mapA (getModelGroup options stats) -<< models
  let groupSelectionIds = map _modelGroupId modelGroups
  let weaponFinder = if selectionId `elem` groupSelectionIds then arr (const []) else getWeaponsShallow groupSelectionIds 1
  weapons <- weaponFinder >>> da "Unit Level Weapons: " >>> arr (excludeWeapons options) -<< el
  let finalModelGroups = (filter (\u -> _modelCount u > 0) . addWargear abilities . addUnitWeapons weapons) modelGroups
  let nonWargearAbilites = filter (not . isWargear) abilities
  script <- scriptFromXml options rosterId name selectionId -<< el
  returnA -< \forceName -> Unit selectionId name forceName stats finalModelGroups nonWargearAbilites weapons script

asRoster :: [Value] -> Value
asRoster values = object ["ObjectStates" .= values]

process :: T.Text -> ModelFinder -> Value -> [Unit] -> IO RosterTranslation
process rosterId modelData baseData units = do
  unitsAndErrors <- retrieveAndModifyUnitJSON rosterId modelData units
  let validUnits = filter (/= []) (fmap (join . rights) unitsAndErrors)
  let invalidUnits = filter (/= []) $ (lefts . join) unitsAndErrors
  let positioned = assignPositionsToUnits zeroPos validUnits
  let unstuck = concatMap (map destick) positioned
  let based = addBase baseData unstuck
  let roster = asRoster based
  return $ RosterTranslation (Just roster) (nub invalidUnits)

zeroStats :: Stats
zeroStats = Stats "" "" "" "" "" "" "" "" ""

createModelDescriptors :: Unit -> [ModelDescriptor]
createModelDescriptors unit = fmap (createModelDescriptor unit) (unit ^. subGroups)

createModelDescriptor :: Unit -> ModelGroup -> ModelDescriptor
createModelDescriptor unit group = ModelDescriptor
                                   (group ^. name)
                                   (T.pack <$> Data.List.sort (fmap _weaponName (group ^. weapons)))
                                   (Just (T.pack <$> Data.List.sort (fmap _abilityName (group ^. abilities))))
                                   (Just (T.pack <$> Data.List.sort (fmap _upgradeName (group ^. upgrades))))


generateRosterNames :: T.Text -> [Unit] -> RosterNamesRequest
generateRosterNames rosterId units = RosterNamesRequest rosterId descriptors where
  descriptors = nub $ concatMap createModelDescriptors units

processRoster :: ScriptOptions -> String -> T.Text -> IO [Unit]
processRoster options xml rosterId = do
  let doc = readString [withParseHTML yes, withWarnings no] xml
  runX $ doc >>> withForceName (findSelectionsRepresentingModels >>> makeUnit options rosterId)

assignmentToPair :: ModelAssignment -> (ModelDescriptor, Vec.Vector Value)
assignmentToPair (ModelAssignment desc (Array val)) = (desc,  val)
assignmentToPair (ModelAssignment desc val) = (desc,  Vec.singleton val)

finder :: HM.HashMap ModelDescriptor (Vec.Vector Value) -> ModelFinder
finder map unit modelGroup = do
  let values = HM.lookup (createModelDescriptor unit modelGroup) map
  let choose vs = if not (null  vs) then
          evalRandIO $ do
                r <- getRandomR (0, length vs - 1)
                return $ Just (vs Vec.! Debug.traceShowId r)
        else
          return Nothing
  join <$> mapM choose values


createTTS :: T.Text -> BaseData -> [Unit] -> RosterNamesResponse -> IO RosterTranslation
createTTS rosterId baseData units (RosterNamesResponse assignments) = result where
  map = HM.fromList (fmap assignmentToPair assignments)
  result = process rosterId (finder map) baseData units
