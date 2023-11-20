--- Allow controlling hammerspoon from CLI
require("hs.ipc")
local superKey     = { "cmd", "alt", "ctrl", "shift" }
local mehKey       = { "alt", "ctrl", "shift" }

local windowTools  = require('ext.window')
local appLauncher  = require('ext.appLauncher')
local reloadConfig = function() hs.reload() end
local chrome       = "Google Chrome"


hs.hotkey.bind(superKey, "C", require('ext.my-utils').getBraveURL)

-- hs.hotkey.bind(superKey, "S", planning)
hs.urlevent.bind("flopmon", require('ext.flipFlopMonitors'))
hs.urlevent.bind("startWork", require('ext.my-utils').startWork)
hs.urlevent.bind("endWork", require('ext.my-utils').shutDownWork)
hs.urlevent.bind("placeWindows", require('ext.my-utils').movePlaces)

local launchEmacs = function()
  appLauncher.smartLaunchOrFocus("Emacs")
end

hs.hotkey.bind(superKey, "pad3", windowTools.splitLowerRight)
hs.hotkey.bind(superKey, "pad9", windowTools.splitUpperRight)
hs.hotkey.bind(superKey, "pad7", windowTools.splitUpperLeft)
hs.hotkey.bind(superKey, "pad1", windowTools.splitLowerLeft)
hs.hotkey.bind(superKey, "pad8", windowTools.splitUpperMiddle)
hs.hotkey.bind(superKey, "pad2", windowTools.splitLowerMiddle)
-- hs.hotkey.bind(superKey, ";", windowTools.splitTwoThirdsRight)
-- hs.hotkey.bind(superKey, "B", windowTools.splitTwoThirdsLeft)
-- hs.hotkey.bind(superKey, "D", require('ext.layout').splitBrowserEditor)
hs.hotkey.bind(superKey, "E", launchEmacs)
-- hs.hotkey.bind(superKey, "F", require('ext.layout').splitBrowserTerm)
-- hs.hotkey.bind(superKey, "H", windowTools.splitLeft)
-- hs.hotkey.bind(superKey, "K", windowTools.maximizeWindow)
-- hs.hotkey.bind(superKey, "I", windowTools.splitUp)
-- hs.hotkey.bind(superKey, "L", windowTools.splitRight)
-- hs.hotkey.bind(superKey, "M", windowTools.splitDown)
-- hs.hotkey.bind(superKey, "U", windowTools.splitMiddle)
-- hs.hotkey.bind(superKey, "T", windowTools.splitMiddle)
hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "Left", windowTools.moveLeft)
hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "Right", windowTools.moveRight)
-- hs.hotkey.bind(superKey, "S", planning)

hs.hotkey.bind(mehKey, "a", require('ext.audio').toggleAudioOutputSource)
hs.hotkey.bind(mehKey, "x", function()
  hs.eventtap.keyStroke({ "alt" }, "x")
end)

hs.hotkey.bind({ "cmd", "ctrl" }, "c", function()
  appLauncher.smartLaunchOrFocus("iTerm2")
end
)
hs.hotkey.bind({ "cmd", "alt", "ctrl" }, "R", reloadConfig)


hs.hotkey.bind({}, "F13", function()
  appLauncher.smartLaunchOrFocus("Spotify")
end
)

hs.hotkey.bind({ "shift" }, "F13", function()
  appLauncher.smartLaunchOrFocus("Slack")
end
)

hs.hotkey.bind({}, "F14", function()
  appLauncher.smartLaunchOrFocus(chrome)
end
)

hs.hotkey.bind({}, "F15", launchEmacs)

hs.alert.show("Config loaded")
