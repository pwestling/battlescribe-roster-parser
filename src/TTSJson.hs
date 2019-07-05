{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TTSJson where

import           Control.Arrow        (first)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Debug.Trace          as Debug
import           System.Directory
import           System.FilePath

data Pos = Pos {posX :: Double, posY :: Double, posZ :: Double}

newtype Model = Model Value

setPos :: AsValue a => Pos -> a -> a
setPos Pos{..}  = setel "posX" posX .
                  setel "posY" posY .
                  setel "posZ" posZ where
    setel k val = key "Transform" . key k._Double .~ val

destick :: AsValue a => a -> a
destick = mkFalse "Grid" . mkFalse "Snap" . mkFalse "Locked" . mkFalse "Sticky" where
    mkFalse k = key k._Bool .~ False


setName :: T.Text -> Value -> Value
setName name = setName' . setInStates setName' where
    setName' =  key "Nickname"._String .~ name

type Idex = HM.HashMap T.Text Value

setInStates :: (Value -> Value) -> Value -> Value
setInStates fn m = m & key "States" . members %~ fn

setDescription :: T.Text -> Value -> Value
setDescription desc = setDesc . setInStates setDesc where
    setDesc = key "Description"._String .~ desc

downcaseKeys :: [(T.Text ,v)] -> [(T.Text ,v)]
downcaseKeys = map $ first T.toLower

loadModels :: IO (HM.HashMap T.Text Value)
loadModels = do
    currentDir <- getCurrentDirectory
    let modelDir = currentDir </> "models"
    jsonPaths <- fmap (modelDir </>) <$> listDirectory modelDir
    jsonBytes <- traverse B.readFile (Debug.traceShowId jsonPaths)
    let fullMappings = fmap (^. _Object) jsonBytes
    let downcasedKeys = map (HM.fromList . downcaseKeys . HM.toList) fullMappings
    return $ HM.unions downcasedKeys


setScript :: T.Text -> Value -> Value
--setScript script = setScript' . setInStates setScript' where
--    setScript' =  key "LuaScript"._String .~ script
setScript ignored = id
