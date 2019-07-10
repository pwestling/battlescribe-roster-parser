ACTIVATED_BUTTON = "rgb(1,0.6,1)|rgb(1,0.4,1)|rgb(1,0.2,1)|rgb(1,0.2,1)"
DEFAULT_BUTTON = "#FFFFFF|#FFFFFF|#C8C8C8|rgba(0.78,0.78,0.78,0.5)"
prodServerURL = "https://backend.battlescribe2tts.net"
serverURL = prodServerURL
version = "1.1"

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
  nextModelTarget = self.UI.getAttribute(id, "modelName")
  nextModelButton = id
end

function onObjectPickUp(colorName, obj)
  if nextModelTarget ~= "" then
    obj.highlightOn({1, 0, 1}, 5)
    self.UI.setAttribute(nextModelButton, "colors", ACTIVATED_BUTTON)
    local bounds = obj.getBoundsNormalized()
    local width = math.max(bounds.size.x, bounds.size.z) * 1.2
    local copy = JSON.decode(obj.getJSON())
    copy.Nickname = nextModelTarget
    copy.States = nil
    copy.Width = width
    local data = {
      name = nextModelTarget,
      descriptor = descriptorMapping[nextModelTarget],
      json = copy,
      width = width
    }
    local jsonData = JSON.encode(data)
    local this = self
    spawnObject(
      {
        type = "Notecard",
        callback_function = function(spawned)
          spawned.setVar("bs2tts-allowed", "true")
          spawned.setName(data.name)
          spawned.setDescription(jsonData)
          this.putObject(spawned)
        end
      }
    )
    nextModelTarget = ""
    nextModelButton = ""
  end
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
      self.takeObject(
        {
          guid = storedDataMapping[name],
          callback_function = function(obj)
            obj.destruct()
          end
        }
      )
    end
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

function submitCode(button)
  print("Code is " .. getCode())
  WebRequest.get(serverURL .. "/roster/" .. getCode() .. "/names", processNames)
  print("Code Submitted")
end

function tabToS(tab)
  local s = "{"
  for k, v in pairs(tab) do
    s = s .. k .. "=" .. tostring(v) .. ","
  end
  s = s .. "}"
  return s
end

function processNames(webReq)
  tempLock()
  if not webReq or webReq.error or webReq.is_error then
    broadcastToAll("Error in web request: No such roster or server error")
    return
  end
  print("Names Retrieved")
  local response = JSON.decode(webReq.text)
  local buttonNames = {}
  for k, v in pairs(response.modelsRequested) do
    local weapons = ""
    for k, v in pairs(v.modelWeapons) do
      if weapons ~= "" then
        weapons = weapons .. ", "
      end
      weapons = weapons .. v
    end
    local name = "Model: " .. v.modelName .. "\nWeapons: " .. weapons
    table.insert(buttonNames, name)
    descriptorMapping[name] = v
  end
  local zOffset = -3
  local xOffset = 3
  local vectors = {}
  local index = 0
  local newButtons = {}
  local heightInc = 220
  local widthInc = 820
  local colHeight = 10
  print("Roster mapping: " .. tabToS(rosterMapping))
  for k, v in pairs(buttonNames) do
    local buttonColor = DEFAULT_BUTTON
    if rosterMapping[v] ~= nil then
      buttonColor = ACTIVATED_BUTTON
    end
    local buttonId = "select " .. v .. " " .. index
    buttonMapping[v] = buttonId
    table.insert(
      newButtons,
      {
        tag = "Button",
        attributes = {
          id = buttonId,
          onClick = "setModel",
          modelName = v,
          padding = 20,
          colors = buttonColor,
          fontSize = 50,
          height = heightInc,
          width = widthInc,
          offsetXY = widthInc * (math.floor(index / colHeight)) .. " " .. -1 * heightInc * (index % colHeight)
        },
        value = v
      }
    )
    index = index + 1
  end
  local panel = {
    tag = "Panel",
    attributes = {
      width = widthInc * ((#buttonNames / colHeight) + 1),
      height = heightInc * colHeight,
      position = "1300 0 -300"
    },
    children = newButtons
  }
  local currentUI = self.UI.getXmlTable()
  self.UI.setXmlTable({currentUI[1], panel})
  self.setVectorLines(vectors)
end

function spawnModelRecur(list, index)
  if index <= #list then
    local v = list[index]
    local relPos = v.Transform
    local thisPos = self.getPosition()
    local adjustedPos = {
      x = thisPos.x + relPos.posX - 20,
      y = thisPos.y + relPos.posY + 4,
      z = thisPos.z + relPos.posZ
    }
    local jv = JSON.encode(v)
    spawnObjectJSON({json = jv, position = adjustedPos})
    Wait.frames(
      function()
        spawnModelRecur(list, index + 1)
      end,
      1
    )
  end
end

function createArmy(player, value, id)
  if not createArmyLock then
    tempLock()
    createArmyLock = true
    self.UI.setAttribute(id, "interactable", "false")
    mappingResponse = {modelAssignments = {}}
    for name, json in pairs(rosterMapping) do
      local assignment = {
        modelJSON = json,
        descriptor = descriptorMapping[name]
      }
      table.insert(mappingResponse.modelAssignments, assignment)
    end
    local jsonToSend = JSON.encode(mappingResponse)
    broadcastToAll("Contacting Server (this may take a minute or two)...")
    WebRequest.put(
      serverURL .. "/roster/" .. getCode(),
      jsonToSend,
      function(req)
        broadcastToAll("Loading Models...")
        if req.is_error then
          broadcastToAll("Error in web request: " .. req.error)
        end
        local response = JSON.decode(req.text)
        local objects = response.roster.ObjectStates
        spawnModelRecur(objects, 1)
        Wait.time(
          function()
            createArmyLock = false
            self.UI.setAttribute(id, "interactable", "true")
          end,
          2
        )
      end
    )
  end
end
