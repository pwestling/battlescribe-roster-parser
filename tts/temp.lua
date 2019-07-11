function onLoad()
  Wait.frames(
    function()
      self.setVar("bs2tts-model", true)
      local id = "bs2tts-ui-load"
      loadUI()
      Timer.destroy(id)
      Timer.create(
        {
          identifier = id,
          function_name = "loadUIs",
          parameters = {},
          delay = 2
        }
      )
    end,
  2)
end

function loadUIs()
  local uistring = Global.getVar("bs2tts-ui-string")
  UI.setXml(UI.getXml() .. uistring)
end

function createUI(uiId, playerColor)
  local guid = self.getGUID()
  local uiString = string.gsub(
                   string.gsub(
                   string.gsub([[ $ui ]], "thepanelid", uiId ),
                                          "theguid", guid),
                                          "thecolor", playerColor)
  return uiString
end

function loadUI()
  local totalUI = ""
  for k, color in pairs(Player.getColors()) do
    totalUI = totalUI .. createUI(createName(color), color)
  end
  local base = ""
  if Global.getVar("bs2tts-ui-string") then
    base = Global.getVar("bs2tts-ui-string")
  end
  Global.setVar("bs2tts-ui-string", base .. totalUI)
end

function onScriptingButtonDown(index, peekerColor)
  local player = Player[peekerColor]
  local name = createName(peekerColor)
  if index == 1 and player.getHoverObject() and player.getHoverObject().getDescription() == self.getDescription() then
    UI.show(name)
  end
end

function closeUI(player, val, id)
  local peekerColor = player.color
  UI.hide(createName(peekerColor))
end

function recursiveUIChangeWithinRecur(attribute, attributeFunction, element)
  if element.attributes and element.attributes.id then
    print("Changing attr for "..element.attributes.id)
    if element.attributes[attribute] then
      UI.setAttribute(element.attributes.id, attribute, attributeFunction(element.attributes[attribute]))
    end
  end
  if element.children then
    for k,v in pairs(element.children) do
      recursiveUIChangeWithinRecur(attribute, attributeFunction, v)
    end
  end
end

function recursiveUIChangeWithin(id, attribute, attributeFunction)
  local xmlTable = UI.getXmlTable()
  for k,v in pairs(xmlTable) do
    if v.attributes and v.attributes.id == id then
      recursiveUIChangeWithinRecur(attribute, attributeFunction, v)
      return
    end
  end
end

function onXmlArray(s, f)
  local result = {}
  for el in string.gmatch(s, "%S+") do
    table.insert(result, f(el))
  end
  return table.concat(result, " ")
end

function minusWidth(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  local scrollView = createName("scrollView")
  print("minusWidth "..panelId)
  recursiveUIChangeWithin(panelId, "width", function(w) tostring(tonumber(w) * 0.8) end)
  recursiveUIChangeWithin(panelId, "columnWidths", function(w)
    onXmlArray(w, function(e) tostring(tonumber(e) * 0.8 end)
  end)
end

function plusWidth(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  local scrollView = createName("scrollView")
  print("plusWidth "..panelId)
  recursiveUIChangeWithin(panelId, "width", function(w) tostring(tonumber(w) * 1.25) end)
  recursiveUIChangeWithin(panelId, "columnWidths", function(w)
    onXmlArray(w, function(e) tostring(tonumber(e) * 1.25 end)
  end)
end

function minusHeight(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  print("minusHeight "..panelId)

end

function plusHeight(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  print("plusHeight"..panelId)
end

function grow(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  print("Grow "..panelId)
end

function shrink(player, val, id)
  local peekerColor = player.color
  local panelId = createName(peekerColor)
  print("Shrink "..panelId)
end

function createName(color)
  local guid = self.getGUID()
  return (guid .. "-" .. color)
end