{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TTSJson where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           System.Directory
import           System.FilePath

data Pos = Pos {posX :: Double, posY :: Double, posZ :: Double}

newtype Model = Model Value

setPos :: AsValue a => Pos -> a -> a
setPos Pos{..}  = setel "posX" posX .
                  setel "posY" posY .
                  setel "posZ" posZ where
    setel k val = key "Transform" . key k._Double .~ val

setName :: (AsValue a) => T.Text -> a -> a
setName name = key "Nickname"._String .~ name

setDescription :: (AsValue a) => T.Text -> a -> a
setDescription desc = key "Description"._String .~ desc

loadModels :: IO (HM.HashMap T.Text Value)
loadModels = do
    currentDir <- getCurrentDirectory
    let modelDir = currentDir </> "models"
    jsonPaths <- fmap (modelDir </>) <$> listDirectory modelDir
    jsonBytes <- traverse B.readFile jsonPaths
    return $ HM.unions $ fmap (^. _Object) jsonBytes

