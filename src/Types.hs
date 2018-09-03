{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens
import qualified Data.Text    as T

data ModelGroup = ModelGroup {
    _name       :: T.Text,
    _modelCount :: Int,
    _stats      :: Maybe Stats,
    _weapons    :: [Weapon]
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
    _unitName :: String, 
    _unitDefaultStats :: Stats,
    _subGroups :: [ModelGroup], 
    _abilities :: [String],
    _unitWeapons :: [Weapon],
    _script :: String} deriving Show

data Weapon = Weapon {
    _weaponName :: String, 
    _range :: String, 
    _type :: String,
    _weaponStrength :: String, 
    _AP :: String, 
    _damage :: String, 
    _special :: String
} deriving Show

makeLenses ''Stats
makeLenses ''ModelGroup
makeLenses ''Unit
