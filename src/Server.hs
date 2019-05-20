{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server where

import           Codec.Archive.Zip
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy        as B
import qualified Data.ByteString.Lazy.Char8  as C8
import           Data.Maybe
import qualified Data.Text                   as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.Cors (simpleCors)
import           Safe                        as X (headMay, headNote, initMay,
                                                   tailMay)
import           Servant
import           Servant.Auth.Server
import           Servant.Multipart
import           Types

type TranslateRosterAPI = "roster" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] RosterTranslation
type AddModelAPI = "model" :> Capture "unitname" T.Text :>
  QueryParam "name" T.Text :> QueryParams "tag" T.Text :> ReqBody '[JSON] Value :> Post '[JSON] ()

type API = TranslateRosterAPI :<|> AddModelAPI

api :: Proxy API
api = Proxy

getUploadZipContents :: MultipartData Mem -> Maybe B.ByteString
getUploadZipContents multipartData = do
  file <- headMay (files multipartData)
  let zipEntries = zEntries $ toArchive $ fdPayload file
  onlyEntry <- headMay zipEntries
  return (fromEntry onlyEntry)

translateRoster :: (User -> String -> IO RosterTranslation) -> MultipartData Mem -> Handler RosterTranslation
translateRoster processor multipartData = do
  let contents = C8.unpack <$> getUploadZipContents multipartData
  if isJust contents
    then liftIO $ processor (User "") (fromJust contents)
    else throwError err400 {
      errBody = "Invalid Roster"
    }

processModelAdd :: (T.Text -> Value -> IO ()) -> T.Text -> Maybe T.Text -> [T.Text] -> Value -> Handler ()
processModelAdd modelAdder unitName modelName tags modelData = do
  let mn = fromMaybe "" modelName
  let taggedKey t = unitName <> "$" <> mn <> "$" <> t
  let keys = unitName : unitName <> "$" <> mn : map taggedKey tags
  let adder key = modelAdder key modelData
  liftIO $ mapM (adder . T.toLower) keys
  return ()

app :: (Database d) => (User -> String -> IO RosterTranslation) -> d -> Application
app processor database = serve api $
    translateRoster processor :<|>
    processModelAdd (addModel database)

startApp :: (Database d) => d -> (ModelFinder -> User -> String -> IO RosterTranslation) -> Int -> IO ()
startApp db processor  port = withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        putStrLn ("Starting server on port " ++ show port)
        runSettings settings $ simpleCors (app (processor (findModel db)) db)
