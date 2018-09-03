{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module TTSUI where

import           Control.Arrow
import           Data.List
import           Data.Monoid
import qualified Data.Text         as T
import qualified Data.Text.Encoding         as E

import qualified Data.Text.IO
import           Debug.Trace       as Debug
import qualified NeatInterpolation as NI
import           Text.XML.HXT.Core
import           TTSJson
import           Types
import Crypto.Hash.MD5
import Data.HexString


scriptFromXml :: ArrowXml a => String -> a XmlTree String
scriptFromXml name = uiFromXML name >>> arr asScript >>> arr T.unpack

uiFromXML :: ArrowXml a => String -> a XmlTree T.Text
uiFromXML name = listA (deep (hasName "profile")) >>> profilesToXML name

mapA :: ArrowXml a => a b c -> a [b] [c]
mapA a = listA (unlistA >>> a)

profilesToXML :: ArrowXml a => String -> a [XmlTree] T.Text
profilesToXML name = proc el -> do
    tabs <- inferTables -< el
    returnA -< masterPanel (T.pack name) (filter tableNotEmpty tabs)

tableNotEmpty :: Table -> Bool
tableNotEmpty Table{..} = not (null rows)

stat :: ArrowXml a => String -> a XmlTree T.Text
stat statName = child "characteristics" /> hasAttrValue "name" (== statName) >>> getAttrValue "value" >>> arr T.pack

rowFetcher :: ArrowXml a => [a XmlTree T.Text] -> a XmlTree [T.Text]
rowFetcher = catA >>> listA

fetchStats :: ArrowXml a => [String] -> [a XmlTree T.Text]
fetchStats names = getAttrValueT "name" : map stat names

inferTables :: ArrowXml a => a [XmlTree] [Table]
inferTables = proc profiles -> do
    profileTypes <- mapA (getAttrValue "profiletypename") >>> arr nub -< profiles
    tables <- listA (catA (map inferTable profileTypes)) -<< profiles
    returnA -< tables

inferTable :: ArrowXml a => String -> a [XmlTree] Table
inferTable profileType = proc profiles -> do
    matchingProfiles <-  mapA (hasAttrValue "profiletypename" (== profileType)) -< profiles
    characteristics <- mapA (this /> hasName "characteristics" /> hasName "characteristic" >>> getAttrValue "name") >>> arr nub -<< matchingProfiles
    let header = map T.pack (profileType : characteristics)
    rows <- mapA (rowFetcher (fetchStats characteristics)) -<< matchingProfiles
    let widths = computeWidths (header : rows)
    let sortedUniqueRows = sort (nubBy (\o t -> head o == head t ) rows)
    returnA -< normalTable header sortedUniqueRows widths

bound :: Double -> Double
bound i =  result where
    thresh = 10
    mult = 0.15
    result = if i < thresh then i * 1.5 else thresh + ((i-thresh) * mult)

computeWidths :: [[T.Text]] -> [Double]
computeWidths vals = widths where
    asLengths = map (map (bound . fromIntegral . T.length)) vals :: [[Double]]
    asLineLengths = map maximum (transpose asLengths) :: [Double]
    avg = sum asLineLengths
    widths = map (/ avg) asLineLengths

asScript :: T.Text -> T.Text
asScript uiRaw = [NI.text|

function createUI()
  local id = [[$id]]
  local buttonId = id 
  local panelId = "panel-" .. buttonId
  local guid = self.getGUID()
  if not UI.getAttribute(panelId, "visibility") then
    local uiString1 = string.gsub([[ $ui ]], "theclosefunction", guid .. "/closeUI")
    local uiString2 = string.gsub(uiString1, "thebuttonid", buttonId)
    local uiString3 = string.gsub(uiString2, "thevisibility", "")
    local uiString = string.gsub(uiString3, "thepanelid", panelId)
    return uiString
  else
    return ""
  end  
end

function onLoad()
  self.setTable("vis",{})
  local counter = Global.getVar("frameCounter")
  if not counter then
    counter = 1
  end
  Global.setVar("frameCounter", counter+1)
  local name = self.getName()
  local update = function ()
    local totalUI = createUI()
    if totalUI ~= "" then  
      UI.setXml(UI.getXml() .. totalUI)
    end
  end
  Wait.frames(update, counter*10)  
end

function onScriptingButtonDown(index, peekerColor)
  local player = Player[peekerColor]
  if index == 1 and player.getHoverObject() and player.getHoverObject().getGUID() == self.getGUID() then
    local id = [[$id]]
    local buttonId = id
    local panelId = "panel-" .. buttonId
    local vis = self.getTable("vis")
    vis[player.color] = player.color
    setVis(panelId, vis)
  end
end

function closeUI(player, val, id)
  local panelId = "panel-"..id
  local vis = UI.getAttribute(panelId, "visibility")
  local peekerColor = player.color 
  local vis = self.getTable("vis")
  vis[player.color] = nil
  setVis(panelId, vis)
end

function setVis(panelId, vis)
  local vistable = {}
  for k,v in pairs(vis) do
    if v then
        table.insert(vistable, v)
    end
  end
  if #vistable > 0 then
    local visstring = table.concat(vistable, "|")
    UI.setAttribute(panelId, "visibility", visstring)  
    UI.setAttribute(panelId, "active", "true")
  else
    UI.setAttribute(panelId, "active", "false")
  end
  self.setTable("vis", vis)
end

|] where
    ui = uiRaw
    id = toText (fromBytes (hash (E.encodeUtf8 ui)))

escapeQuotes :: String -> String
escapeQuotes (c : s) = if c == '"' then "&quot;" ++ escapeQuotes s else c : escapeQuotes s
escapeQuotes [] = []

escapeQuotesT :: T.Text -> T.Text
escapeQuotesT = T.pack . escapeQuotes . T.unpack

masterPanel :: T.Text -> [Table] -> T.Text
masterPanel name tables = [NI.text|
    <Panel id="thepanelid" visibility="thevisibility" active="false" width="$width" height="$height" returnToOriginalPositionWhenReleased="false" allowDragging="true" color="#FFFFFF" childForceExpandWidth="false" childForceExpandHeight="false">
    <TableLayout autoCalculateHeight="true" width="$width" childForceExpandWidth="false" childForceExpandHeight="false">
    <Row preferredHeight="40">
    <Text fontSize="25" text="$name" width="$width"/>
    <Button id="thebuttonid" class="topButtons" rectAlignment="UpperRight" color="#990000" textColor="#FFFFFF" text="X" height="40" width="40" onClick="theclosefunction" />
    </Row>
    <Row preferredHeight="$scrollHeight">
    <VerticalScrollView scrollSensitivity="30" height="$scrollHeight" width="$width">
    <TableLayout padding="10" cellPadding="5" horizontalOverflow="Wrap" columnWidths="$width" autoCalculateHeight="true">
    $tableXml
    </TableLayout>
    </VerticalScrollView>
    </Row>
    </TableLayout>
    </Panel> |] where
        heightN = 400
        height = numToT heightN
        scrollHeight = numToT (heightN - 40)
        widthN = 600
        width = numToT widthN
        tableXml = mconcat $ map (tableToXml widthN) tables

data Table = Table {
    columnWidthPercents :: [Double],
    headerHeight        :: Integer,
    rowHeight           :: Integer,
    textSize            :: Integer,
    headerTextSize      :: Integer,
    header              :: [T.Text],
    rows                :: [[T.Text]]
} deriving Show

normalTable header rows widths = Table widths 40 80 15 20 header rows

numToT :: Integer -> T.Text
numToT = T.pack . show

tableToXml :: Integer -> Table -> T.Text
tableToXml tableWidth Table{..} = [NI.text|
  <Row preferredHeight="$tableHeight">
    <TableLayout autoCalculateHeight="false" cellPadding="5" columnWidths="$colWidths">
        $headerText
        $bodyText
    </TableLayout>
  </Row>
|] where
    tableHeight = numToT $ headerHeight + (rowHeight * fromIntegral (length rows))
    colWidths = T.intercalate " " $ map (numToT . floor . (* fromIntegral tableWidth)) columnWidthPercents
    headHeight = numToT headerHeight
    rHeight = numToT rowHeight
    tSize = numToT textSize
    htSize = numToT headerTextSize
    headerText = tRow htSize "Bold" headHeight header
    bodyText = mconcat $ map (tRow tSize "Normal" rHeight . map escapeQuotesT) rows

tCell :: T.Text -> T.Text -> T.Text -> T.Text
tCell fs stl val = [NI.text| <Cell><Text resizeTextForBestFit="true" resizeTextMaxSize="$fs" resizeTextMinSize="12"
  text="$val" fontStyle="$stl" fontSize="$fs"/></Cell> |]

tRow :: T.Text -> T.Text -> T.Text -> [T.Text] -> T.Text
tRow fs stl h vals = "<Row preferredHeight=\"" <> h <> "\">" <> mconcat (map (tCell fs stl) vals) <> "</Row>"

child :: ArrowXml a => String -> a XmlTree XmlTree
child tag = getChildren >>> hasName tag

getAttrValueT :: ArrowXml a => String -> a XmlTree T.Text
getAttrValueT attr = getAttrValue attr >>> arr T.pack
