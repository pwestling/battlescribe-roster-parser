{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Codec.Archive.Zip
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as B
import qualified Data.ByteString.Lazy.Char8           as C8
import           Data.Either.Combinators
import qualified Data.HashMap.Strict                  as HM
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Text.Encoding
import           Data.UUID
import           Data.UUID.V4
import           Database.Redis
import qualified Debug.Trace                          as Debug
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger
import           RosterProcessing
import           Safe                                 as X (headMay, headNote,
                                                            initMay, tailMay)
import           Servant
import           Servant.Multipart
import           TTSJson
import           Types






type CreateRosterAPI = "roster" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] RosterId
type GetRosterAPI = "roster" :> Capture "id" T.Text :> "names" :> Get '[JSON] RosterNamesRequest
type CompleteRosterAPI = "roster" :> Capture "id" T.Text :> ReqBody '[JSON, PlainText, OctetStream] RosterNamesResponse :> Put '[JSON] RosterTranslation

instance FromJSON a => MimeUnrender PlainText a where
  mimeUnrender _ = Data.Aeson.eitherDecode

instance FromJSON a => MimeUnrender OctetStream a where
    mimeUnrender _ = Data.Aeson.eitherDecode

type API = CreateRosterAPI :<|> GetRosterAPI :<|> CompleteRosterAPI

api :: Proxy API
api = Proxy

getUploadZipContents :: MultipartData Mem -> Maybe B.ByteString
getUploadZipContents multipartData = do
  file <- headMay (files multipartData)
  let zipEntries = zEntries $ toArchive $ fdPayload file
  onlyEntry <- headMay zipEntries
  return (fromEntry onlyEntry)

createRoster :: Connection -> MultipartData Mem -> Handler RosterId
createRoster redisConn multipartData = do
  let contents = C8.unpack <$> getUploadZipContents multipartData
  fullUUID <- liftIO nextRandom
  let id = T.take 8 $ toText fullUUID
  case contents of
    Just xml -> liftIO $ do
      unitData <- processRoster xml id
      storeInRedis redisConn id unitData
      return $ RosterId id
    Nothing -> throwError err400 {
      errBody = "Invalid Roster"
    }

getRoster :: Connection -> T.Text -> Handler RosterNamesRequest
getRoster conn id =  do
  maybeUnits <- liftIO $ retrieveFromRedis conn id
  case maybeUnits of
    Just units -> return $ generateRosterNames id units
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Roster with ID " <> id
    }

completeRoster :: Connection -> BaseData -> T.Text -> RosterNamesResponse -> Handler RosterTranslation
completeRoster conn baseData id mapping =  do
  units <- liftIO $ retrieveFromRedis conn id
  case units of
    Just result -> return $ createTTS baseData result mapping
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Roster with ID " <> id
    }

storeInRedis :: ToJSON j => Connection -> T.Text -> j -> IO ()
storeInRedis redisConn id struct = do
  let json = toJSON struct
  runRedis redisConn (setex (encodeUtf8 id) 1209600 (C8.toStrict (encode json)))
  return ()

retrieveFromRedis :: FromJSON j => Connection -> T.Text -> IO (Maybe j)
retrieveFromRedis redisConn id = do
  json <- runRedis redisConn (get (encodeUtf8 id))
  let asMaybe = join $ rightToMaybe json
  let decoded = fmap (join . decode' . C8.fromStrict) asMaybe
  return $ join decoded

app :: Connection -> BaseData -> Application
app conn baseData = serve api $
  createRoster conn :<|>
  getRoster conn :<|>
  completeRoster conn baseData

addContentType :: Request -> Request
addContentType req = req { requestHeaders = header : requestHeaders req} where
  header = ("Content-Type", "application/json")

startApp :: String -> Int -> Int -> IO ()
startApp redisHost redisPort appPort = withStdoutLogger $ \aplogger -> do
        modelData <- loadModels
        conn <- connect defaultConnectInfo
        let baseData = fromJust $ HM.lookup "base" modelData
        let settings = setPort appPort $ setLogger aplogger defaultSettings
        putStrLn ("Starting server on port " ++ show appPort)
        runSettings settings $ logStdoutDev (simpleCors (app conn baseData))

main :: IO ()
main = startApp "ignored" 0 8080
