{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import qualified Data.Text                  as T
import           RosterProcessing
import           System.Environment
import           TTSJson
import           Types


main :: IO ()
main = do
  [f] <- getArgs
  xml     <- readFile f
  let id = "deadbeef"
  modelData <- loadModels
  let baseData = fromJust $ HM.lookup "base" modelData
  unitData <- liftIO $ processRoster (ScriptOptions True Nothing Nothing) xml id
  let nameRequest = generateRosterNames id unitData
  let modelNames = _modelsRequested nameRequest
  let dummyAssignments = fmap (\d -> ModelAssignment d baseData) modelNames
  let ttsData = createTTS id baseData unitData (RosterNamesResponse dummyAssignments)
  putStrLn (C8.unpack (encode ttsData))
  return ()
