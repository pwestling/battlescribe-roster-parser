{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}


module Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.Text           as T

import           GHC.Generics
import           Servant.Auth.Server

data ModelGroup = ModelGroup {
    _name       :: T.Text,
    _modelCount :: Int,
    _stats      :: Maybe Stats,
    _weapons    :: [Weapon],
    _abilities  :: [String]
} deriving Show

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
} deriving Show
data Unit = Unit {
    _unitName         :: String,
    _forceName        :: String,
    _unitDefaultStats :: Stats,
    _subGroups        :: [ModelGroup],
    _unitAbilities    :: [String],
    _unitWeapons      :: [Weapon],
    _script           :: String} deriving Show

data Weapon = Weapon {
    _weaponName     :: String,
    _range          :: String,
    _type           :: String,
    _weaponStrength :: String,
    _AP             :: String,
    _damage         :: String,
    _special        :: String
} deriving Show

makeLenses ''Stats
makeLenses ''ModelGroup
makeLenses ''Unit

newtype User = User String deriving (Eq, Show, Read, Generic)
instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

type ModelFinder = User -> Unit -> ModelGroup -> IO (Maybe Value)
type BaseData = Value

data RosterTranslation = RosterTranslation {
    _roster       :: Maybe Value,
    _missingUnits :: [String]
} deriving Generic

makeLenses ''RosterTranslation
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''RosterTranslation

class Database d where
    findModel :: d -> User -> Unit -> ModelGroup -> IO (Maybe Value)
    addModel :: d -> T.Text -> Value -> IO ()

