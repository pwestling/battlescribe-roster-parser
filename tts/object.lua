ACTIVATED_BUTTON = "rgb(1,0.6,1)|rgb(1,0.4,1)|rgb(1,0.2,1)|rgb(1,0.2,1)"
DEFAULT_BUTTON = "#FFFFFF|#FFFFFF|#C8C8C8|rgba(0.78,0.78,0.78,0.5)"
prodServerURL = "https://backend.battlescribe2tts.net"
serverURL = prodServerURL
version = "1.6"

nextModelTarget = ""
nextModelButton = ""
descriptorMapping = {}
code = ""
rosterMapping = {}
buttonMapping = {}
storedDataMapping = {}
createArmyLock = false

function onScriptingButtonDown(index, peekerColor)
  local player = Player[peekerColor]
  if index == 1 and player.getHoverObject() and player.getHoverObject().getGUID() == self.getGUID() then
    broadcastToAll("Activating Development Mode")
    serverURL = "http://localhost:8080"
  end
  if index == 2 and player.getHoverObject() and player.getHoverObject().getGUID() == self.getGUID() then
    broadcastToAll("Activating Production Mode")
    serverURL = prodServerURL
  end
end

function tempLock()
  self.setLock(true)
  local this = self
  Wait.time(
    function()
      this.setLock(false)
    end,
    3
  )
end

function onLoad()
  local contained = self.getObjects()
  for k, v in pairs(contained) do
    local name = v.name
    local data = JSON.decode(v.description)
    rosterMapping[name] = data.json
    descriptorMapping[name] = data.descriptor
    storedDataMapping[name] = v.guid
  end
  checkVersion()
  Wait.time(announce, 4)
end

function announce()
  broadcastToAll("Thanks for using Battlescribe Army Creator! Go to https://battlescribe2tts.net for instructions")
end

function checkVersion()
  WebRequest.get(serverURL .. "/version", verifyVersion)
end

function verifyVersion(req)
  if req and req.text then
    local json = JSON.decode(req.text)
    if json and json.id then
      local remoteVersion = json.id
      if remoteVersion ~= version then
        Wait.time(
          function()
            broadcastToAll(
              "You are using an out-of-date version of Battlescribe Army Creator. " ..
                "Get the latest version from the workshop!"
            )
          end,
          3
        )
      end
    end
  end
end

function setModel(player, value, id)
  nextModelTarget = self.UI.getValue(id)
  print("Target is " .. nextModelTarget)
  local shortName = self.UI.getAttribute(id, "shortName")
  nextModelButton = id
  broadcastToAll("Pick up an object to set it as the model for " .. shortName)
end

pickedUp = {}
timerId = nil

function onObjectPickUp(colorName, obj)
  if nextModelTarget ~= "" then
    obj.highlightOn({1, 0, 1}, 5)
    self.UI.setAttribute(nextModelButton, "colors", ACTIVATED_BUTTON)
    table.insert(pickedUp, obj)
    if timerId ~= nil then
      Wait.stop(timerId)
    end
    timerId = Wait.frames(function() 
      processPickups()
    end,10)
  end
end

function processPickups()
  local modelList = {}
  print("Processing " .. tostring(#pickedUp) .. " pickups for " .. nextModelTarget)
  for k, obj in pairs(pickedUp) do
    local bounds = obj.getBoundsNormalized()
    local width = math.max(bounds.size.x, bounds.size.z) * 1.2
    local copy = JSON.decode(obj.getJSON())
    copy.Nickname = nextModelTarget
    copy.States = nil
    copy.Width = width
    table.insert(modelList, copy)
  end
  print("Desc: " .. JSON.encode(descriptorMapping[nextModelTarget]))
  local data = {
    name = nextModelTarget,
    descriptor = descriptorMapping[nextModelTarget],
    json = modelList
  }
  local jsonData = JSON.encode(data)
  local this = self
  local nameCopy = nextModelTarget .. ""
  spawnObject(
    {
      type = "Notecard",
      callback_function = function(spawned)
        spawned.setVar("bs2tts-allowed", "true")
        spawned.setName(nameCopy)
        spawned.setDescription(jsonData)
        this.putObject(spawned)
      end
    }
  )
  nextModelTarget = ""
  nextModelButton = ""
  pickedUp = {}
  timerId = nil
end

function filterObjectEnter(obj)
  return obj.getVar("bs2tts-allowed") == true
end

function onObjectLeaveContainer(thisContainer, takenObject)
  if thisContainer.getGUID() == self.getGUID() then
    tempLock()
    local name = takenObject.getName()
    rosterMapping[name] = nil
    storedDataMapping[name] = nil
    if buttonMapping[name] ~= nil then
      thisContainer.UI.setAttribute(buttonMapping[name], "colors", DEFAULT_BUTTON)
    end
  end
end

function onObjectEnterContainer(thisContainer, addedObject)
  if thisContainer.getGUID() == self.getGUID() then
    tempLock()
    local name = addedObject.getName()
    local data = JSON.decode(addedObject.getDescription())
    if storedDataMapping[name] ~= nil then
      print("Removing old entry for: " .. name)
      self.takeObject(
        {
          guid = storedDataMapping[name],
          callback_function = function(obj)
            obj.destruct()
          end
        }
      )
    end
    print("Adding descriptor for " .. name .. ": " .. JSON.encode(data.descriptor))
    descriptorMapping[name] = data.descriptor
    rosterMapping[name] = data.json
    storedDataMapping[name] = addedObject.guid
    if buttonMapping[name] ~= nil then
      thisContainer.UI.setAttribute(buttonMapping[name], "colors", ACTIVATED_BUTTON)
    end
  end
end

function setCode(player, value, id)
  code = value
end

function getCode()
  return code
end

function submitCode(player, value, id)
  if player.host then
    WebRequest.get(serverURL .. "/roster/" .. getCode() .. "/names", processNames)
  else
    broadcastToAll("Sorry, only the host of this game may use the Battlescribe Army Creator")
  end
end

function tabToS(tab)
  local s = "{"
  for k, v in pairs(tab) do
    s = s .. k .. "=" .. tostring(v) .. ","
  end
  s = s .. "}"
  return s
end

function getList(header, l)
  local result = ""
  for k, v in pairs(l) do
    if result ~= "" then
      result = result .. ", "
    end
    result = result .. v
  end
  if #result > 0 then
    result = "\n" .. header .. ": " .. result
  end
  if #result > 128 then
    result = string.sub(result, 1, 90) .. "..."
  end
  return result
end

function processNames(webReq)
  tempLock()
  if not webReq or webReq.error or webReq.is_error then
    broadcastToAll("Error in web request: No such roster or server error")
    return
  end
  local response = JSON.decode(webReq.text)
  local buttonNames = {}
  local shortNames = {}
  for k, v in pairs(response.modelsRequested) do
    local weapons = getList("Weapons",v.modelWeapons)
    local abilities = getList("Abilities",v.modelAbilities)
    local upgrades = getList("Upgrades",v.modelUpgrades)
    local name = "Model: " .. v.modelName .. weapons .. upgrades .. abilities 
    table.insert(buttonNames, { name = name, lineHeight = lineHeight})
    shortNames[name] = v.modelName
    descriptorMapping[name] = v
    print("Desc for " .. name .. " is " .. JSON.encode(v))
  end
  local zOffset = -3
  local xOffset = 3
  local vectors = {}
  local index = 0
  local newButtons = {}
  local heightInc = 400
  local widthInc = 900
  local colHeight = 5
  if #buttonNames > 25 then
    colHeight = 8
  end
  if #buttonNames > 40 then
    colHeight = 10
  end
  for k, v in pairs(buttonNames) do
    local buttonColor = DEFAULT_BUTTON
    if rosterMapping[v] ~= nil then
      buttonColor = ACTIVATED_BUTTON
    end
    local buttonId = "select " .. v["name"] .. " " .. index
    buttonMapping[v] = buttonId
    table.insert(
      newButtons,
      {
        tag = "Button",
        attributes = {
          id = buttonId,
          onClick = "setModel",
          modelName = v["name"],
          shortName = shortNames[v["name"]],
          padding = 20,
          colors = buttonColor,
          fontSize = 50,
          height = heightInc,
          width = widthInc,
          offsetXY = (widthInc * (math.floor(index / colHeight))) .. " " .. -1 * heightInc * (index % colHeight)
        },
        value = v["name"]
      }
    )
    index = index + 1
  end
  local xstart = 1300 + (widthInc - 900)
  local panel = {
    tag = "Panel",
    attributes = {
      width = widthInc * ((#buttonNames / colHeight) + 1),
      height = heightInc * colHeight,
      position = tostring(xstart) .. " 0 -300"
    },
    children = newButtons
  }
  local currentUI = self.UI.getXmlTable()
  self.UI.setXmlTable({currentUI[1], panel})
  self.setVectorLines(vectors)
end

function spawnModelRecur(id, threads, limit, index)
  if index < limit then
    WebRequest.get(
      serverURL .. "/v2/roster/" .. id .. "/" .. index,
      function(req)
        if req and req.text then
          local v = JSON.decode(req.text)
          local relPos = v.Transform
          local thisPos = self.getPosition()
          local adjustedPos = {
            x = thisPos.x + relPos.posX - 20,
            y = thisPos.y + relPos.posY + 4,
            z = thisPos.z + relPos.posZ
          }
          local jv = JSON.encode(v)
          spawnObjectJSON(
            {
              json = jv,
              position = adjustedPos
            }
          )
          spawnModelRecur(id, threads, limit, index + 1)
        else
          broadcastToAll("Error requesting model " .. index)
        end
      end
    )
  else
    spawnThreadCounter = spawnThreadCounter + 1
    if spawnThreadCounter >= threads then
      broadcastToAll("Army creation complete!")
    end
  end
end

spawnThreadCounter = 0

function createArmy(player, value, id)
  if player.host then
    if not createArmyLock then
      spawnThreadCounter = 0
      tempLock()
      createArmyLock = true
      Wait.time(
        function()
          createArmyLock = false
          self.UI.setAttribute(id, "interactable", "true")
        end,
        5
      )
      self.UI.setAttribute(id, "interactable", "false")
      mappingResponse = {modelAssignments = {}}
      for name, json in pairs(rosterMapping) do
        local assignment = {
          modelJSON = json,
          descriptor = descriptorMapping[name]
        }
        print(JSON.encode(descriptorMapping[name]))
        table.insert(mappingResponse.modelAssignments, assignment)
      end
      local jsonToSend = JSON.encode(mappingResponse)
      broadcastToAll("Contacting Server (this may take a minute or two)...")
      WebRequest.put(
        serverURL .. "/v2/roster/" .. getCode(),
        jsonToSend,
        function(req)
          broadcastToAll("Loading Models...")
          if not req or req.is_error then
            broadcastToAll("Error in web request")
          end
          local status, result =
            pcall(
            function()
              return JSON.decode(req.text)
            end
          )
          if status then
            local response = JSON.decode(req.text)
            local itemsToSpawn = response.itemCount
            local groupsOf = 10
            for i = 0, (itemsToSpawn / groupsOf), 1 do
              local start = i * groupsOf
              spawnModelRecur(getCode(), (itemsToSpawn / groupsOf), math.min(start + groupsOf, itemsToSpawn), start)
            end
          else
            broadcastToAll("Got error: " .. req.text, {r = 1, g = 0, b = 0})
          end
        end
      )
    end
  else
    broadcastToAll("Sorry, only the host of this game may use the Battlescribe Army Creator")
  end
end
