module Main where

import           Safe
import           System.Environment
import           Text.XML.HXT.Core


 --getUnits :: Filter a b

main = do
  args <- getArgs
  let fileToParse = head args
  html <- readFile "test.ros"
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> hasName "a" >>> getAttrValue "href"
  mapM_ putStrLn links
