{-# LANGUAGE OverloadedStrings #-}

module Databases where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text           as T
import           Safe
import           Types

newtype HashMapDB = HMDB (HM.HashMap T.Text Value)

instance Database HashMapDB where
  findModel (HMDB db) = lookupModel (`HM.lookup` db)

concatText :: T.Text -> [T.Text] -> Maybe T.Text
concatText delim l = result where
  nonEmptyElements = filter (not . T.null) l
  result = if null nonEmptyElements then
    Nothing
    else
    Just $ foldl1' (\s1 s2 -> s1 <> delim <> s2) nonEmptyElements

generateLookupKeys :: User -> Unit -> ModelGroup -> [T.Text]
generateLookupKeys (User un) unit modelGroup =
  lookups where
    username = [T.pack un]
    modelName =  [modelGroup ^. name, ""]
    uName = [T.pack $ unit ^. unitName, ""]
    fName = [T.pack $ unit ^. forceName, ""]
    tags = map T.pack $ map _weaponName (unit ^. unitWeapons)
                        ++ map _weaponName (modelGroup ^. weapons)
                        ++ unit ^. unitAbilities
                        ++ modelGroup ^. abilities
    product = map (^..each) (sequence [username, fName, uName, modelName, "" : tags])
    lookups = map T.toLower (mapMaybe (concatText "$") product)

lookupModel :: (T.Text -> Maybe Value) -> ModelFinder
lookupModel mapping user unit modelGroup = pure result where
  lookupKeys = generateLookupKeys user unit modelGroup
  result = headMay (mapMaybe mapping lookupKeys)

lookupModelIO :: (T.Text -> IO (Maybe Value)) -> ModelFinder
lookupModelIO mapping user unit modelGroup = do
  let lookupKeys = generateLookupKeys user unit modelGroup
  let findingsInIO = map mapping lookupKeys
  findings <- sequence findingsInIO
  return $ headMay (catMaybes findings)
