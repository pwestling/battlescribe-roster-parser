{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server where

import           Codec.Archive.Zip
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString                      as SBS
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
import           System.Environment
import           TTSJson
import           Types


version :: Version
version = Version "1.10"

type CreateRosterAPI = "roster"
                       :> QueryFlag "addScripts" :> QueryParam "uiWidth" Int :> QueryParam "uiHeight" Int :> QueryParam "modelsToFind" String :> QueryParam "modelsToConsolidate" String
                       :> QueryFlag "excludeGrenades" :> QueryFlag "excludeSidearms" :> QueryFlag "excludeAbilities" 
                       :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] RosterId
type GetRosterAPI = "roster" :> Capture "id" T.Text :> "names" :> Get '[JSON] RosterNamesRequest
type CompleteRosterAPI = "roster" :> Capture "id" T.Text :> ReqBody '[JSON, PlainText, OctetStream] RosterNamesResponse :> Put '[JSON] RosterTranslation
type CompleteRosterAPIV2 = "v2" :> "roster" :> Capture "id" T.Text :> ReqBody '[JSON, PlainText, OctetStream] RosterNamesResponse :> Put '[JSON] ItemCount
type IterateRosterAPIV2 = "v2" :> "roster" :> Capture "id" T.Text :> Capture "index" Int :>  Get '[JSON] Value
type VersionAPI = "version" :> Get '[JSON] Version

instance FromJSON a => MimeUnrender PlainText a where
  mimeUnrender _ = Data.Aeson.eitherDecode

instance FromJSON a => MimeUnrender OctetStream a where
    mimeUnrender _ = Data.Aeson.eitherDecode

type API = CreateRosterAPI
         :<|> GetRosterAPI
         :<|> CompleteRosterAPI
         :<|> VersionAPI
         :<|> CompleteRosterAPIV2
         :<|> IterateRosterAPIV2

api :: Proxy API
api = Proxy

getUploadZipContents :: MultipartData Mem -> Maybe B.ByteString
getUploadZipContents multipartData = do
  file <- headMay (files multipartData)
  let zipEntries = zEntries $ toArchive $ fdPayload file
  onlyEntry <- headMay zipEntries
  return (fromEntry onlyEntry)

createRoster :: Connection -> Bool -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Bool -> Bool -> Bool -> MultipartData Mem -> Handler RosterId
createRoster redisConn addScripts uiWidth uiHeight modelsToFind modelsToConsolidate excludeGrenades excludeSidearms excludeAbilities multipartData = do
  let contents = C8.unpack <$> getUploadZipContents multipartData
  fullUUID <- liftIO nextRandom
  let id = T.take 8 $ toText fullUUID
  case contents of
    Just xml -> do
      unitData <- liftIO $ processRoster (ScriptOptions addScripts uiWidth uiHeight excludeGrenades excludeSidearms excludeAbilities modelsToFind modelsToConsolidate) xml id
      storeInRedis redisConn id unitData
      return $ RosterId id
    Nothing -> throwError err400 {
      errBody = "Invalid Roster"
    }

getRoster :: Connection -> T.Text -> Handler RosterNamesRequest
getRoster conn id =  do
  maybeUnits <- retrieveFromRedis conn id
  case maybeUnits of
    Just units -> return $ generateRosterNames id units
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Roster with ID " <> id
    }

completeRoster :: Connection -> BaseData -> T.Text -> RosterNamesResponse -> Handler RosterTranslation
completeRoster conn baseData id mapping =  do
  units <- retrieveFromRedis conn id
  case units of
    Just result -> do
      ttsData <- liftIO $ createTTS id baseData result mapping
      storeInRedis conn (id <> "-saved-roster") ttsData
      return ttsData
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Roster with ID " <> id
    }

completeRosterV2 :: Connection -> BaseData -> T.Text -> RosterNamesResponse -> Handler ItemCount
completeRosterV2 conn baseData id mapping =  do
  units <- retrieveFromRedis conn id
  case units of
    Just result -> do
      ttsData <- liftIO $ createTTS id baseData result (Debug.trace (C8.unpack (encode mapping)) mapping)
      let numItems = fmap (length . (^. key "ObjectStates"._Array)) (ttsData ^. roster)
      storeInRedis conn (id <> "-saved-roster") ttsData
      return $ ItemCount (fromMaybe 0 numItems)
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Roster with ID " <> id
    }

getRosterItem :: Connection -> T.Text -> Int -> Handler Value
getRosterItem conn id index =  do
  rosterTranslation <- retrieveFromRedis conn (id <> "-saved-roster")
  case rosterTranslation of
    Just result -> do
      let maybeRoster = fromMaybe undefined (result ^. roster)
      let desiredItem = maybeRoster ^? key "ObjectStates" . nth index
      case desiredItem of
        Just item -> return item
        Nothing -> throwError err400 {
          errBody = C8.fromStrict $ encodeUtf8 $ "No Item " <> T.pack (show index) <> " in roster " <> id <> id
        }
    Nothing -> throwError err400 {
      errBody = C8.fromStrict $ encodeUtf8 $ "No Saved Roster with ID " <> id
    }

storeInRedis :: ToJSON j => Connection -> T.Text -> j -> Handler ()
storeInRedis redisConn id struct = do
  let json = toJSON struct
  liftIO $ runRedis redisConn (setex (encodeUtf8 id) 3600 (C8.toStrict (encode json)))
  return ()

retrieveFromRedis :: FromJSON j => Connection -> T.Text -> Handler (Maybe j)
retrieveFromRedis redisConn id = do
  json <- liftIO $  runRedis redisConn (get (encodeUtf8 id))
  let asMaybe = join $ rightToMaybe json
  let decoded = fmap (join . decode' . C8.fromStrict) asMaybe
  return $ join decoded

app :: Connection -> BaseData -> Application
app conn baseData = serve api $
  createRoster conn :<|>
  getRoster conn :<|>
  completeRoster conn baseData :<|>
  return version :<|>
  completeRosterV2 conn baseData :<|>
  getRosterItem conn


startApp :: String -> Int -> Int -> IO ()
startApp redisHost redisPort appPort = withStdoutLogger $ \aplogger -> do
        modelData <- loadModels
        conn <- connect $ defaultConnectInfo {
          connectHost = redisHost,
          connectPort = PortNumber (fromIntegral redisPort)}
        let baseData = fromJust $ HM.lookup "base" modelData
        let settings = setPort appPort $ setLogger aplogger defaultSettings
        putStrLn ("Starting server on port " ++ show appPort)
        runSettings settings $  logStdoutDev (simpleCors (app conn baseData))

main :: IO ()
main = do
  appPortMay <- lookupEnv "PORT"
  redisPortMay <- lookupEnv "REDIS_PORT"
  redisHostMay <- lookupEnv "REDIS_HOST"
  let appPort = maybe 8080 read appPortMay
  let redisPort = maybe 6379 read redisPortMay
  let redisHost = fromMaybe "localhost" redisHostMay
  startApp redisHost redisPort appPort
