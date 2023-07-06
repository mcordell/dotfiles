local cache    = {
  bindings    = {},
  launchTimer = nil
}
local module   = { cache = cache }
local appTools = require('ext.appTools')
local spaces   = require('ext.spaces')
local v        = appTools.vars


module.setFantasticalToToday = function(fantastical)
  fantastical:mainWindow():raise()
  fantastical:mainWindow():focus()
  hs.eventtap.keyStroke({ "cmd" }, "1")
  hs.eventtap.keyStroke({ "cmd" }, "t")
end

module.shutDownWork = function()
  for _, app in ipairs({ v.slack, v.zoom, v.teams, v.outlook }) do
    local application = hs.application.get(app)
    if application ~= nil then
      application:kill()
    end
  end
end

module.startWork = function()
  for _, app in ipairs({ v.slack, v.zoom, v.teams, v.fantastical, v.outlook }) do
    hs.application.open(app)
  end
end

module.movePlaces = function()
  local spaceMap = {}
  spaceMap[v.teams] = spaces.vars.comToolsSpace
  spaceMap[v.zoom] = spaces.vars.comToolsSpace
  spaceMap[v.outlook] = spaces.vars.comToolsSpace
  spaceMap[v.slack] = spaces.vars.mainLapSpace
  spaceMap[v.fantastical] = spaces.vars.mainLapSpace
  for app, spaceid in pairs(spaceMap) do
    window = hs.application.get(app):mainWindow()

    if (window ~= nil) then
      hs.spaces.moveWindowToSpace(window, spaceid)
    end
  end
end

module.getBraveURL = function()
  local source = [[
      tell application "Brave Browser"
      activate
      set theURL to URL of active tab of front window
      set the clipboard to theURL & return
      end tell
      ]]

  hs.osascript.applescript(source)
end


return module
