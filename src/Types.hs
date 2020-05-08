{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}


module Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T


import           GHC.Generics
import           Servant.Auth.Server

data Ability = Ability{
    _abilityName :: String,
    _id          :: String,
    _description :: String
} deriving (Show, Eq, Generic)

data Upgrade = Upgrade{
    _upgradeName :: String,
    _id          :: String
} deriving (Show, Eq, Generic)

data ModelGroup = ModelGroup {
    _modelGroupId :: String,
    _name         :: T.Text,
    _modelCount   :: Int,
    _stats        :: Maybe Stats,
    _weapons      :: [Weapon],
    _abilities    :: [Ability],
    _upgrades     :: [Upgrade]

} deriving (Show, Eq)

data Stats = Stats {
    _move       :: String,
    _ws         :: String,
    _bs         :: String,
    _strength   :: String,
    _toughness  :: String,
    _wounds     :: String,
    _attacks    :: String,
    _leadership :: String,
    _save       :: String
} deriving (Show, Eq)

data Unit = Unit {
    _unitSelectionId  :: String,
    _unitName         :: String,
    _forceName        :: String,
    _unitDefaultStats :: [(String, Stats)],
    _subGroups        :: [ModelGroup],
    _unitAbilities    :: [Ability],
    _unitWeapons      :: [Weapon],
    _script           :: String
} deriving (Show, Eq)


data Weapon = Weapon {
    _weaponName     :: String,
    _range          :: String,
    _type           :: String,
    _weaponStrength :: String,
    _AP             :: String,
    _damage         :: String,
    _special        :: String,
    _count          :: Int,
    _id             :: String
} deriving (Show, Eq, Generic)

instance Hashable Weapon

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Ability
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Stats
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Weapon
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Upgrade
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ModelGroup
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Unit

makeLenses ''Stats
makeLenses ''ModelGroup
makeLenses ''Unit

type ModelFinder = Unit -> ModelGroup -> IO (Maybe Value)
type BaseData = Value

data RosterTranslation = RosterTranslation {
    _roster       :: Maybe Value,
    _missingUnits :: [String]
} deriving Generic

makeLenses ''RosterTranslation
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''RosterTranslation

data ModelDescriptor = ModelDescriptor {
    --_unitName     :: T.Text,
    _modelName      :: T.Text,
    _modelWeapons   :: [T.Text],
    _modelAbilities :: Maybe [T.Text],
    _modelUpgrades  :: Maybe [T.Text]


} deriving (Generic, Eq, Show)

instance Hashable ModelDescriptor

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ModelDescriptor

data RosterNamesRequest = RosterNamesRequest {
    _rosterId        :: T.Text,
    _modelsRequested :: [ModelDescriptor]
} deriving Generic

makeLenses ''RosterNamesRequest
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''RosterNamesRequest

data ModelAssignment = ModelAssignment {
    _descriptor :: ModelDescriptor,
    _modelJSON  :: Value
} deriving (Generic, Show)

makeLenses ''ModelAssignment
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ModelAssignment

newtype RosterNamesResponse = RosterNamesResponse {
    _modelAssignments :: [ModelAssignment]
} deriving (Generic, Show)

makeLenses ''RosterNamesResponse
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''RosterNamesResponse

newtype RosterId = RosterId {
    _id :: T.Text
}

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''RosterId

newtype Version = Version {
    _id :: T.Text
}

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Version


newtype ItemCount = ItemCount {
    _itemCount :: Int
}

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ItemCount

data ScriptOptions = ScriptOptions {
    shouldAddScripts :: Bool,
    uiWidth          :: Maybe Int,
    uiHeight         :: Maybe Int
}
