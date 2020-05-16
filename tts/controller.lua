
ui = [[
  <Panel color="#FFFFFF" height="1000" width="450" position = "0 500 -100">
    <VerticalLayout> 
      <Text fontSize="50" color="rgb(0,0,0)">BS2TTS Control Panel</Text>
      <Button height="160" color="rgb(1,1,1)" onClick="theguid/openDataSheet"><Text fontSize="50" width="600" color="rgb(0,0,0)" >Open Datasheet</Text></Button>
      <Text fontSize="45" color="rgb(0,0,0)">Wounds</Text>
      <HorizontalLayout>
        <Button color="rgb(1,1,1)" onClick="theguid/decreaseWounds"><Text fontSize="70" color="rgb(0,0,0)" >-</Text></Button>
        <Button color="rgb(1,1,1)" onClick="theguid/increaseWounds"><Text fontSize="70" color="rgb(0,0,0)" >+</Text></Button>
      </HorizontalLayout>
      <Text fontSize="45" color="rgb(0,0,0)">Range Finder</Text>
      <HorizontalLayout>
        <Button color="rgb(1,1,1)" onClick="theguid/decreaseRadius"><Text fontSize="70" color="rgb(0,0,0)" >-</Text></Button>
        <Button color="rgb(1,1,1)" onClick="theguid/increaseRadius"><Text fontSize="70" color="rgb(0,0,0)" >+</Text></Button>
      </HorizontalLayout>
      <Text fontSize="45" color="rgb(0,0,0)">Base Size</Text>
      <HorizontalLayout>
        <Button color="rgb(1,1,1)" onClick="theguid/decrBase"><Text fontSize="70" color="rgb(0,0,0)" >-</Text></Button>
        <Button color="rgb(1,1,1)" onClick="theguid/incrBase"><Text fontSize="70" color="rgb(0,0,0)" >+</Text></Button>
      </HorizontalLayout>
    </VerticalLayout> 
  </Panel>
]]

function onLoad()
  local completeUi = string.gsub(ui, "theguid", self.getGUID())
  self.UI.setXml(completeUi)
end


function openDataSheet(player, val, id)
  activateScriptingButton(player, 1)
end

function decreaseWounds(player, val, id)
  activateScriptingButton(player, 2)
end

function increaseWounds(player, val, id)
  activateScriptingButton(player, 3)
end

function increaseRadius(player, val, id)
  activateScriptingButton(player, 4)
end

function decreaseRadius(player, val, id)
  activateScriptingButton(player, 5)
end

function incrBase(player, val, id)
  activateScriptingButton(player, 6)
end

function decrBase(player, val, id)
  activateScriptingButton(player, 7)
end


function activateScriptingButton(player, index)
  local peekerColor = player.color
  local selected = player.getSelectedObjects()
  if #selected > 0 then
    for k, obj in pairs(getAllObjects()) do
      if obj.getVar("isMaster") then
        obj.call("onScriptingButtonDownTable", {index = index, peekerColor = peekerColor})
      end
    end
  end
end