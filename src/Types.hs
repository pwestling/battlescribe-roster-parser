{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens
import qualified Data.Text    as T

data ModelGroup = ModelGroup {
    _name       :: T.Text,
    _modelCount :: Int,
    _stats      :: Stats
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
newtype Unit = Unit { _subGroups :: [ModelGroup]} deriving Show

makeLenses ''Stats
makeLenses ''ModelGroup
makeLenses ''Unit
