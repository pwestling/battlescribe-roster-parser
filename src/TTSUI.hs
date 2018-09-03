{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module TTSUI where

import           Control.Arrow
import           TTSJson
import           Types
import           Text.XML.HXT.Core
import qualified Data.Text            as T
import Data.Monoid
import qualified NeatInterpolation as NI
import qualified Data.Text.IO
import Debug.Trace as Debug
import Data.List


scriptFromXml :: ArrowXml a => String -> a XmlTree String
scriptFromXml name = proc el -> do
    ui <- uiFromXML name -< el
    let script = asScript ui
    returnA -< T.unpack script

uiFromXML :: ArrowXml a => String -> a XmlTree T.Text
uiFromXML name = listA (deep (hasName "profile")) >>> profilesToXML name

mapA :: ArrowXml a => a b c -> a [b] [c]
mapA a = listA (unlistA >>> a)

tables :: ArrowXml a => [a [XmlTree] Table]
tables = [getUnitsTable, getWeaponsTable, getPsykerTable,getPowerTable, getAbilitiesTable]

profilesToXML :: ArrowXml a => String -> a [XmlTree] T.Text
profilesToXML name = proc el -> do
    --tabs <- listA (catA tables) -< el
    tabs <- inferTables -< el
    returnA -< masterPanel (T.pack name) (filter tableNotEmpty tabs)

tableNotEmpty :: Table -> Bool
tableNotEmpty Table{..} = not (null rows)

toTable :: ArrowXml a => String -> [T.Text] -> [Double] -> [a XmlTree T.Text] -> a [XmlTree] Table
toTable profileTypeName header widths rowArr = 
    mapA (hasAttrValue "profiletypename" (== profileTypeName)) >>>
    mapA (rowFetcher rowArr) >>>
    arr (\rows -> normalTable header rows widths )

rowFetcher :: ArrowXml a => [a XmlTree T.Text] -> a XmlTree [T.Text]
rowFetcher = catA >>> listA

fetchStats :: ArrowXml a => [String] -> [a XmlTree T.Text]
fetchStats names = getAttrValueT "name" : map stat names

getAbilitiesTable :: ArrowXml a => a [XmlTree] Table
getAbilitiesTable = toTable "Abilities" ["Ability", "Description"] [0.33, 0.66] (fetchStats ["Description"])
    
getWeaponsTable :: ArrowXml a => a [XmlTree] Table
getWeaponsTable = toTable "Weapon" ["Weapon", "Range", "Type", "S", "AP", "D", "Abilities"]
                                   [0.19, 0.10, 0.14, 0.03, 0.04, 0.03, 0.43] 
                                   (fetchStats ["Range", "Type", "S", "AP", "D", "Abilities"])

getUnitsTable :: ArrowXml a => a [XmlTree] Table
getUnitsTable = toTable "Unit" ["Unit", "M", "WS", "BS", "S", "T", "W", "A", "Ld", "Save"]
                               [0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.2]
                               (fetchStats ["M", "WS", "BS", "S", "T", "W", "A", "Ld", "Save"])

getPsykerTable :: ArrowXml a => a [XmlTree] Table
getPsykerTable = toTable "Psyker" ["Psyker", "Cast", "Deny", "Powers Known", "Other"]
                                [0.3, 0.05, 0.05, 0.3, 0.3]
                                (fetchStats ["Cast", "Deny", "Powers Known", "Other"])

getPowerTable :: ArrowXml a => a [XmlTree] Table
getPowerTable = toTable "Psychic Power" ["Power", "Warp Charge", "Range", "Details"]
                                [0.3, 0.1, 0.1, 0.5]
                                (fetchStats ["Warp Charge", "Range", "Details"])

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
    returnA -< normalTable header (sort (nubBy (\o t -> head o == head t ) rows)) widths

average :: [Int] -> Double
average i = fromIntegral (sum i) / fromIntegral (length i)

bound :: Double -> Double
bound i =  result where
    thresh = 10
    mult = 0.2
    histo = if i < thresh then i * 1.4 else thresh + ((i-thresh) * mult)
    result = histo

-- computeWidths :: [[T.Text]] -> [Double]
-- computeWidths vals = widths where
--     asLengths = map (map (bound . T.length)) vals :: [[Int]]
--     asLineLengths = map sum asLengths :: [Int]
--     avg = average asLineLengths
--     transposed = transpose vals
--     avgColSize = map (average . map (bound . T.length)) transposed
--     widths = map (/ avg) avgColSize

computeWidths :: [[T.Text]] -> [Double]
computeWidths vals = widths where
    asLengths = map (map (bound . fromIntegral . T.length)) vals :: [[Double]]
    asLineLengths = map maximum (transpose asLengths) :: [Double]
    avg = sum asLineLengths
    widths = map (/ avg) asLineLengths
    

stat :: ArrowXml a => String -> a XmlTree T.Text
stat statName = child "characteristics" /> hasAttrValue "name" (== statName) >>> getAttrValue "value" >>> arr T.pack

asScript :: T.Text -> T.Text
asScript uiRaw = [NI.text|

function createUI(color)
  local id = self.getGUID()
  local buttonId = id .. "-" .. color
  local panelId = "panel-" .. buttonId
  local uiString1 = string.gsub([[ $ui ]], "theclosefunction", id .. "/closeUI")
  local uiString2 = string.gsub(uiString1, "thebuttonid", buttonId)
  local uiString3 = string.gsub(uiString2, "thevisibility", color)
  local uiString = string.gsub(uiString3, "thepanelid", panelId)
  local globalXml = UI.getXml()
  if globalXml then
    UI.setXml(globalXml ..  uiString)
  else
    UI.setXml(uiString)
  end
end

function onScriptingButtonDown(index, peekerColor)
  print("Player hit " .. tostring(index))
  local player = Player[peekerColor]
  if index == 1 and player.getHoverObject() and player.getHoverObject().getGUID() == self.getGUID() then
    print("Making UI visible")
    local id = self.getGUID()
    local buttonId = id .. "-" .. peekerColor
    local panelId = "panel-" .. buttonId
    local exists = UI.getAttribute(panelId, "active")
    if not exists then
        createUI(peekerColor)
    else
      UI.setAttribute(panelId, "active", "true")
    end
  end
end

function closeUI(player, val, id)
  print("Closed")
  local panelId = "panel-"..id
  UI.setAttribute(panelId, "active", "false")
end

|] where
    ui = uiRaw

escapeQuotes :: String -> String
escapeQuotes (c : s) = if c == '"' then "&quot;" ++ escapeQuotes s else c : escapeQuotes s
escapeQuotes [] = []

escapeQuotesT :: T.Text -> T.Text
escapeQuotesT = T.pack . escapeQuotes . T.unpack

wrapTag :: T.Text -> T.Text -> T.Text
wrapTag t v = "<" <> t <> ">" <> v <> "</" <> t <> ">" 

alternate :: (a -> c) -> (a -> c) -> [a] -> [c]
alternate = alternateEven where
    alternateEven f1 f2 (h : l) = f1 h : alternateOdd f1 f2 l
    alternateEven _ _ [] = []
    alternateOdd f1 f2 (h : l) = f2 h : alternateEven f1 f2 l
    alternateOdd _ _ [] = []

wrapTagAttr :: T.Text -> [T.Text] -> T.Text -> T.Text
wrapTagAttr t attrs v = "<" <> t <> mconcat (alternate (\s -> " " <> s <> "=") (\s -> "\"" <> s <>"\"") attrs) 
  <> ">" <> v <> "</" <> t <> ">" 

row :: T.Text -> T.Text
row = wrapTag "Row"

cell :: T.Text -> T.Text
cell = wrapTag "Cell"

text :: T.Text -> T.Text
text = wrapTag "Text"

str :: String -> T.Text
str = wrapTag "Text" . T.pack

panelHeader :: T.Text -> T.Text -> T.Text
panelHeader rawName id = [NI.text| 
  <Row>
  <Text id="WindowTitle" text="Stats: $name" class="UIText" rectAlignment="UpperCenter" alignment="LowerCenter" width="230" />
  <Button id="$id-closeButton" class="topButtons" rectAlignment="UpperRight" color="#990000" textColor="#FFFFFF" text="X" onClick="theclosefunction" />
  </Row>


  |] where name = rawName

table = wrapTag "TableLayout"
panel = wrapTag "Panel"
                           
masterPanel :: T.Text -> [Table] -> T.Text                                  
masterPanel name tables = [NI.text|
    <Panel id="thepanelid" visibility="thevisibility" active="true" width="$width" height="$height" returnToOriginalPositionWhenReleased="false" allowDragging="true" color="#FFFFFF" childForceExpandWidth="false" childForceExpandHeight="false"> 
    <TableLayout autoCalculateHeight="true" width="$width" childForceExpandWidth="false" childForceExpandHeight="false">
    <Row preferredHeight="40">
    <Text fontSize="25" text="$name" width="$width"/>
    <Button id="thebuttonid" class="topButtons" rectAlignment="UpperRight" color="#990000" textColor="#FFFFFF" text="X" height="40" width="40" onClick="theclosefunction" />
    </Row>
    <Row preferredHeight="$scrollHeight">
    <VerticalScrollView height="$scrollHeight" width="$width">
    <TableLayout padding="10" cellPadding="5" horizontalOverflow="Wrap" columnWidths="$width" autoCalculateHeight="true">
    $tableXml
    </TableLayout>
    </VerticalScrollView>
    </Row>
    </TableLayout>
    </Panel> |] where
        heightN = 500
        height = numToT heightN
        scrollHeight = numToT (heightN - 40)
        widthN = 900
        width = numToT widthN
        tableXml = mconcat $ map (tableToXml widthN) tables

data Table = Table {
    columnWidthPercents :: [Double],
    headerHeight :: Integer,
    rowHeight :: Integer,
    textSize :: Integer, 
    headerTextSize :: Integer,
    header :: [T.Text],
    rows :: [[T.Text]]
} deriving Show

normalTable header rows widths = Table widths 40 80 17 20 header rows

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