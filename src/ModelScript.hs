{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}


module ModelScript where

import qualified NeatInterpolation  as NI
import qualified Data.Text          as T


descriptionId = "desc-id"

descriptionScript :: T.Text -> T.Text -> T.Text
descriptionScript rosterId unitId = [NI.text|
function onLoad()
  self.setVar("$descriptionId", "$uniqueId")
end
|] where
  uniqueId = rosterId <> ":" <> unitId


createScript :: T.Text -> T.Text -> T.Text
createScript ui uniqueId = [NI.text|
  
  function uiSub(uiTable, target, value)
    if uiTable["attributes"] then
      for k,v in pairs(uiTable["attributes"]) do
        uiTable["attributes"][k] = nil
        uiTable["attributes"][string.gsub(k, target, value)] = string.gsub(v, target, value)
      end
    end
    if uiTable["children"] then
      for k,child in pairs(uiTable["children"]) do
        uiTable["children"][k] = uiSub(child, target, value)
      end
    end
    return uiTable
  end

  function createUI(uiId, playerColor)
    local guid = self.getGUID()
    local uiTable = uiSub(
                    uiSub(
                    uiSub($ui, "thepanelid", uiId),
                                "theguid", guid),
                                "thevisibility", playerColor)
    return uiTable
  end

  function onLoad()
    self.setVar("$descriptionId", desc())
    self.setVar("isMaster", true)
    for k,v in pairs(getAllObjects()) do
      if v.getVar("$descriptionId") == desc() and v.getVar("isMaster") and v.getGUID() ~= self.getGUID() then
        if v.getPosition().x > 9999 and v.getPosition().z > 9999 then
          log("Destroying previous master for this unit: " .. desc())
          v.destruct()
        else
          log("Usurping previous master for this unit: " .. desc())
          v.setVar("isMaster", false)
        end
      end
    end
  end

  function copyUITable(uiTable)
    local result = {}
    for k, element in pairs(uiTable) do
      local newElement = {}
      newElement["tag"] = element["tag"]
      if element["value"] ~= nil then
        newElement["value"], times = string.gsub(element["value"],"[ ]+"," ")
      end
      newElement["attributes"] = element["attributes"]
      newElement["children"] = {}
      if element["children"] ~= nil then
        newElement["children"] = copyUITable(element["children"])
      end
      table.insert(result, newElement)
    end
    return result
  end


  function loadUI(playerColor)
    local panelId = createName(playerColor)
    local uiTable = createUI(panelId, playerColor)
    local currentUI = UI.getXmlTable()
    local inserted = false
    for k,element in pairs(currentUI) do
      if element["attributes"] ~= nil and element["attributes"]["id"] == panelId then
        currentUI[k]["children"] = uiTable["children"]
        inserted = true
      end
    end
    if not inserted then
      table.insert(currentUI, uiTable)
    end
    local result = copyUITable(currentUI)
    UI.setXmlTable(result)
  end

  function unloadUI(playerColor)
    local panelId = createName(playerColor)
    local uiTable = UI.getXmlTable()
    local panelIndex = -1
    for index, element in pairs(uiTable) do
      if element["attributes"]["id"] == panelId then
        panelIndex = index
        break
      end
    end
    if panelIndex >= 0 then
      local el = uiTable[panelIndex]
    end
    uiCreated[playerColor] = false
  end

  uiCreated = {}

  timesActivated = 0

  validBaseMillis = {
    {x = 25, z = 25},
    {x = 30, z = 30},
    {x = 32, z = 32},
    {x = 40, z = 40},
    {x = 50, z = 50},
    {x = 55, z = 55},
    {x = 60, z = 60},
    {x = 100, z = 100},
    {x = 25, z = 25},
    {x = 25, z = 75},
    {x = 75, z = 25},
    {x = 120, z = 92},
    {x = 92, z = 120},
    {x = 170, z = 105},
    {x = 105, z = 170},
  }

  function assignBase(inc, model)
   if Global.getTable("bs2tts-saved-bases") == nil then
    Global.setTable("bs2tts-saved-bases", {})
   end
   local baseMap = Global.getTable("bs2tts-saved-bases")
   local meshFile = model.getCustomObject().mesh
   if baseMap[meshFile] == nil then
    log("Initing base map")
    baseMap[meshFile] = 0
   end
   local index = baseMap[meshFile]
   local newIndex = (index + inc) % #validBaseMillis
   log("Base map index is now: " .. tostring(newIndex))
   log("Base millis are: " .. tostring(validBaseMillis[newIndex+1]["x"]) .. " " .. tostring(validBaseMillis[newIndex+1]["z"]))
   baseMap[meshFile] = newIndex
   Global.setTable("bs2tts-saved-bases", baseMap)
  end

  function determineBase(model)
    local chosenBase =  {x = 25, z = 25}
    local milliToInch = 0.0393701
    
    if Global.getTable("bs2tts-saved-bases") and Global.getTable("bs2tts-saved-bases")[model.getCustomObject().mesh] then
      chosenBase = validBaseMillis[Global.getTable("bs2tts-saved-bases")[model.getCustomObject().mesh] + 1]
      chosenBase = {x = (chosenBase.x * milliToInch)/2, z = (chosenBase.z * milliToInch)/2}
    else
      local bounds = model.getBoundsNormalized()
      local xBounds = bounds.size.x
      local zBounds = bounds.size.z
      local closestSum = 10000000000
      log(bounds)
      for k, base in pairs(validBaseMillis) do
        local baseInchX = (milliToInch - 0.001) * base.x
        local baseInchZ = (milliToInch - 0.001) * base.z
        if xBounds > baseInchX and zBounds > baseInchZ then
          local distSum = (xBounds - baseInchX) + (zBounds - baseInchZ)
          if distSum < closestSum then
            closestSum = distSum
            chosenBase = base
          end
        end
      end
      log(chosenBase)
      if chosenBase == nil then
        chosenBase = {x = xBounds/2, z = zBounds/2}
      else
        chosenBase = {x = (chosenBase.x * milliToInch)/2, z = (chosenBase.z * milliToInch)/2}
      end
    end
    return chosenBase
  end

  function onScriptingButtonDownTable(params)
    onScriptingButtonDown(params.index, params.peekerColor)
  end

  function onScriptingButtonDown(index, peekerColor)
    local player = Player[peekerColor]
    local name = createName(peekerColor)
    if (self.getVar("isMaster") and player.getHoverObject() and player.getHoverObject().getVar("$descriptionId") == desc()) or
        (#player.getSelectedObjects() > 0 and player.getSelectedObjects()[1].getVar("$descriptionId") == desc()) then
      local target = player.getHoverObject() or  player.getSelectedObjects()[1]
      if index == 1 then
          loadUI(peekerColor)
          Wait.frames(function()
            updateModelCount()
            UI.setAttribute(createName(peekerColor), "active", true)
          end, 2)
      end
      if index == 2 or index == 3 then
        local inc = index == 2 and -1 or 1
        local name = target.getName()
        local current, total = string.gmatch(name,"([0-9]+)/([0-9]+)")()
        current = math.max(tonumber(current) + inc,0)
        local newName = string.gsub(name, "([0-9]+)/([0-9]+)", tostring(current) .. "/" .. total)
        target.setName(newName)
      end
      if index == 4 or index == 5 or index == 8 then
        local inc = index == 4 and 1 or -1
        if index == 8 then
          inc = 0
        end
        if target.getVar("bs2tts-aura-circle") == nil then
          target.setVar("bs2tts-aura-circle", 0)
        end
        local newRadius = math.max(target.getVar("bs2tts-aura-circle") + inc,0)
        target.setVar("bs2tts-aura-circle", newRadius)
        local circ = {}
        local base = {}
        local baseRadiuses = determineBase(target)
        if newRadius > 0 then
          circ = getCircleVectorPoints(newRadius, baseRadiuses.x, baseRadiuses.z, target)
          base = getCircleVectorPoints(0, baseRadiuses.x, baseRadiuses.z, target)
        end

        target.setVectorLines({
          {
              points    = circ,
              color     = highlighting or {1,0,1},
              thickness = 0.125 * 1/(target.getScale().x),
              rotation  = {0,0,0},
          },
          {
              points    = base,
              color     = highlighting or {1,0,1},
              thickness = 0.1 * 1/(target.getScale().x),
              rotation  = {0,0,0},
          }
        })
        broadcastToAll("Measuring "..tostring(newRadius).."\"")
      end
      if index == 6 or index == 7 then
        local inc = 1
        if index == 7 then
          inc = -1
        end
        assignBase(inc, target)
        onScriptingButtonDown(8, peekerColor)
      end
    end
  end

  function onObjectDrop(playerColor, obj)
    scheduleUpdateIfInUnit(obj)
  end

  function onObjectDestroy(obj)
    if obj.getGUID() ~= self.getGUID() then
      scheduleUpdateIfInUnit(obj)
    end
  end

  function scheduleUpdateIfInUnit(obj)
  if obj.getVar("$descriptionId") == desc() then
    collectUnitModels()
    local id = desc() .. "countModels"
    Timer.destroy(id)
    Timer.create(
    {
      identifier = id,
      function_name = "updateModelCount",
      parameters = {},
      delay = 0.2
    }
  )
  end
  end

  function distance2D(point1, point2)
    local x = point1.x - point2.x
    local z = point1.z - point2.z
    return math.sqrt(x * x + z * z)
  end

  unitModels = nil
  unitModelCount = 0
  unitUprightModelCount = 0

  function collectUnitModels()
    unitModels = {}
    unitModelCount = 0
    unitUprightModelCount = 0
    for k,v in pairs(getAllObjects()) do
      if v.getVar("$descriptionId") == desc() then
        table.insert(unitModels, v)
        unitModelCount = unitModelCount + 1
        if not v.is_face_down then
          unitUprightModelCount = unitUprightModelCount + 1
        end
      end
    end
    log("Collected " .. tostring(unitUprightModelCount) .. " upright models for this unit")
  end

  function operateOnModels(fn)
    if not unitModels then
      collectUnitModels()
    end
    local originModel = nil
    local dist = 100000000000
    for k, model in pairs(unitModels) do
      local newDist = distance2D({x=0,y=0,z=0}, model.getPosition())
      if not model.is_face_down and newDist < dist then
        originModel = model
        dist = newDist
      end
    end
    local seenModels = {}
    searchModels(originModel, seenModels, fn)
  end

  function updateModelCount()
    collectUnitModels()
    local modelCounts = {}
    if unitModels then
      for k, model in pairs(unitModels) do
        model.highlightOff()
      end
    end

    local getModelNames = function(model)
      if not modelCounts[model.getName()] then
        modelCounts[model.getName()] = 0
      end
      modelCounts[model.getName()] = modelCounts[model.getName()] + 1
      if highlighting then
        model.highlightOn(highlighting)
      end
    end

    operateOnModels(getModelNames)
    local label = ""
    local keys = {}
    for k in pairs(modelCounts) do table.insert(keys, k) end
    table.sort(keys)

    for index,k in pairs(keys) do
      local v = modelCounts[k]
      modelName = string.gsub(k, "[0-9]+/[0-9]+","")
      label = label .. modelName .. " - " .. tostring(v) .. "\n"
    end
    local theid = self.getGUID() .. "-modelcount"
    UI.setAttribute(theid, "text", label )
  end

  function getCenterDist(obj)
    local boundsSize = obj.getBoundsNormalized().size
    local longest = math.max(boundsSize.x, boundsSize.z)
    return longest/2
  end

  function arrToS(a)
    result = "[] "
    for i, k in pairs(a) do
      result = result .. tostring(k) .. ","
    end
    result = result .. " ]"
    return result
  end

  function searchModels(origin, seen, fn)
    local modelsInRange = 0
    local rangeModelGUIDS = {}
    seen[origin.getGUID()] = true
    for k, model in pairs(unitModels) do
      if not model.is_face_down and model.getGUID() ~= origin.getGUID() then
        local originCenterDist = getCenterDist(origin)
        local modelCenterDist = getCenterDist(model)
        local dist = distance2D(origin.getPosition(), model.getPosition())
        if dist < (2.05 + originCenterDist + modelCenterDist) then
          modelsInRange = modelsInRange + 1
          table.insert(rangeModelGUIDS, model.getGUID())
          if (unitUprightModelCount >= 6 and modelsInRange == 2) or (unitUprightModelCount < 6 and modelsInRange == 1) then
            log("Model " .. tostring(origin.getGUID()) .. " is in range of " .. tostring(modelsInRange) .. " models, namely: " .. arrToS(rangeModelGUIDS))
            fn(origin)
          end
          if not seen[model.getGUID()] then
            searchModels(model, seen, fn, modelCount)
          end
        end
      end
    end
  end

  highlighting = false

  function highlightUnitRed() highlightUnit("Red") end
  function highlightUnitGreen() highlightUnit("Green") end
  function highlightUnitBlue() highlightUnit("Blue") end
  function highlightUnitPurple() highlightUnit("Purple") end
  function highlightUnitYellow() highlightUnit("Yellow") end
  function highlightUnitWhite() highlightUnit("White") end
  function highlightUnitOrange() highlightUnit("Orange") end
  function highlightUnitTeal() highlightUnit("Teal") end
  function highlightUnitPink() highlightUnit("Pink") end

  function highlightUnitNone()
    highlighting = false
    updateModelCount()
  end

  function highlightUnit(color)
    if highlighting ~= color then
      highlighting = color
    else
      highlighting = false
    end
    updateModelCount()
  end

  function onDestroy()
    local id = desc() .. "countModels"
    Timer.destroy(id)
    collectUnitModels()
    if #unitModels > 1 then
      local newobj = self.clone({position = {x = 10000, y = 10000, z = 10000}})
      newobj.setLock(true)
    end
    for k,v in pairs(Player.getColors()) do
      UI.hide(createName(v))
    end
  end

  function closeUI(player, val, id)
    local peekerColor = player.color
    UI.setAttribute(createName(peekerColor), "active", false)
  end

  function desc()
    return "$uniqueId"
  end

  function createName(color)
    local guid = self.getGUID()
    return "bs2tts" .. "-" .. color
  end

  function rotateVector2d(vec, degrees)
    local sin,cos,toRads = math.sin, math.cos, math.rad
    local result = {
      x = vec.x * cos(toRads(degrees)) - vec.z * sin(toRads(degrees)),
      z = vec.x * sin(toRads(degrees)) + vec.z * cos(toRads(degrees)),
      y = vec.y
    }
    return result
  end

  function getCircleVectorPoints(radius,baseX, baseZ, obj)
      local result = {}
      local scaleFactor = 1/obj.getScale().x
      local rotationDegrees =  obj.getRotation().y
      local steps = 64
      local degrees,sin,cos,toRads = 360/steps, math.sin, math.cos, math.rad
      for i = 0,steps do
          table.insert(result,{
              x = cos(toRads(degrees*i))*((radius+baseX)*scaleFactor),
              z = sin(toRads(degrees*i))*((radius+baseZ)*scaleFactor),
              y = 1
          })
      end
      return result
  end

  |]