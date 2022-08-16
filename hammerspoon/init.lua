--- Allow controlling hammerspoon from CLI
require("hs.ipc")

local highlightWindow = require('ext.drawing').highlightWindow
local template        = require('ext.template')
log             = hs.logger.new('init', 'debug')
local windowTools     = require('ext.window')
local reloadConfig    = function()
  hs.reload()
end
local appLauncher     = require('ext.appLauncher')
local getApp          = function (appName)
  hs.application.enableSpotlightForNameSearches(true)
  return hs.appfinder.appFromName(appName)
end
local chrome = "Google Chrome"
local superKey = {"cmd", "alt", "ctrl", "shift"}
local mehKey = {"alt", "ctrl", "shift"}
hs.loadSpoon("GlobalMute")

local setFantasticalToToday = function(fantastical)
  fantastical:mainWindow():raise()
  fantastical:mainWindow():focus()
  hs.eventtap.keyStroke({"cmd"}, "1")
  hs.eventtap.keyStroke({"cmd"}, "t")
end

local tripleSplit = function(leftAppName, middleAppName, rightApp)
  hs.application.launchOrFocus(leftAppName)
  hs.application.launchOrFocus(middleAppName)
  hs.application.launchOrFocus(rightApp)
  local left = getApp(leftAppName)
  local mid = getApp(middleAppName)
  local right = getApp(rightApp)

  left:mainWindow():raise()
  mid:mainWindow():raise()
  right:mainWindow():raise()
  mid:mainWindow():focus()
  hs.layout.apply({
      {left, nil, mainScreen, leftThird, nil, nil},
      {mid, nil, mainScreen, middleThird, nil, nil},
      {right, nil, mainScreen, rightThird, nil, nil},
  })
end

local planning = function()
  local fant = "Fantastical"
  hs.application.launchOrFocus(fant)
  local fantastical = getApp(fant)
  setFantasticalToToday(fantastical)
  tripleSplit(fant, "MailMate", "Emacs")
end

local notion = function()
  tripleSplit("Emacs", "Notion", chrome)
end

hs.hotkey.bind(superKey, "C", function()
    source = [[
      tell application "Brave Browser"
      activate
      set theURL to URL of active tab of front window
      set the clipboard to theURL & return
      end tell
      ]]

    hs.osascript.applescript(source)
end
)

leftScreen = nil
rightScreen = nil
mainLaptopScreen = nil
mainScreen = nil

xMin = nil

for screen, position in pairs(hs.screen.screenPositions()) do
  if screen:name() == "Color LCD" then
    mainLaptopScreen = screen
  elseif screen:name() == "DELL U3818DW" then
      mainScreen = screen
  elseif xMin == nil then
    xMin = position.x
    leftScreen = screen
  elseif xMin < position.x then
    rightScreen = screen
  elseif xMin > position.x then
    rightScreen = leftScreen
    leftScreen = screen
  end
end

if mainScreen == nil then
  mainScreen = mainLaptopScreen
end

if mainScreen == nil then
  log.df("Main screen %s", "is empty")
else
  log.df("Main screen %s", mainScreen:name())
end

rightBottom = hs.geometry.unitrect(0.5, 0.5, 0.5, 0.5)
rightTop = hs.geometry.unitrect(0.5, 0, 0.5, 0.5)
leftTop = hs.geometry.unitrect(0, 0, 1, 0.5)
leftBotton = hs.geometry.unitrect(0, 0.5, 1, 0.5)
leftThird = hs.geometry.unitrect(0, 0, .333, 1)
middleThird = hs.geometry.unitrect(.333, 0, .333, 1)
rightThird = hs.geometry.unitrect(.666, 0, .333, 1)
hs.hotkey.bind(superKey, "S", planning)
hs.hotkey.bind(superKey, "N", notion)
hs.urlevent.bind("flopmon", require('ext.flipFlopMonitors'))

local setAudioOutput = function (name)
	local device = hs.audiodevice.findDeviceByName(name)
	if device ~= nil then
		device:setDefaultOutputDevice()
	end
end

local toggleAudioOutputSource = function()
	local current = hs.audiodevice.defaultOutputDevice():name()
	if current == 'Audioengine HD3' then
		setAudioOutput('USB Audio DAC   ')
	elseif current == 'USB Audio DAC   ' then
		setAudioOutput('Audioengine HD3')
	end
end

local splitBrowserTerm = function()
    windowTools.splitMainFocus(getApp(chrome), getApp("iTerm2"))
end

local splitBrowserEditor = function()
	windowTools.splitMainFocus(getApp(chrome), getApp("Emacs"))
end

hs.hotkey.bind(superKey, "pad3", windowTools.splitLowerRight)
hs.hotkey.bind(superKey, "pad9", windowTools.splitUpperRight)
hs.hotkey.bind(superKey, "pad7", windowTools.splitUpperLeft)
hs.hotkey.bind(superKey, "pad1", windowTools.splitLowerLeft)
hs.hotkey.bind(superKey, "pad8", windowTools.splitUpperMiddle)
hs.hotkey.bind(superKey, "pad2", windowTools.splitLowerMiddle)
hs.hotkey.bind(superKey, ";", windowTools.splitTwoThirdsRight)
hs.hotkey.bind(superKey, "B", windowTools.splitTwoThirdsLeft)
hs.hotkey.bind(superKey, "H", windowTools.splitLeft)
hs.hotkey.bind(superKey, "I", windowTools.splitUp)
hs.hotkey.bind(superKey, "L", windowTools.splitRight)
hs.hotkey.bind(superKey, "M", windowTools.splitDown)
hs.hotkey.bind(superKey, "U", windowTools.splitMiddle)
hs.hotkey.bind(superKey, "K", windowTools.maximizeWindow)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", windowTools.moveLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", windowTools.moveRight)
hs.hotkey.bind(superKey, "S", planning)
hs.hotkey.bind(superKey, "N", notion)

hs.hotkey.bind(mehKey, "a", toggleAudioOutputSource)
hs.hotkey.bind(mehKey, "x", function()
	hs.eventtap.keyStroke({"alt"}, "x")
end)

hs.hotkey.bind({"cmd", "ctrl"}, "c", function()
    appLauncher.smartLaunchOrFocus("iTerm2")
                                     end
)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", reloadConfig)

hs.hotkey.bind(superKey, "F", splitBrowserTerm)
hs.hotkey.bind(superKey, "D", splitBrowserEditor)

hs.hotkey.bind({}, "F13", function()
    appLauncher.smartLaunchOrFocus("Spotify")
end
)

hs.hotkey.bind({"shift"}, "F13", function()
    appLauncher.smartLaunchOrFocus("Slack")
end
)

hs.hotkey.bind({}, "F14", function()
    appLauncher.smartLaunchOrFocus(chrome)
end
)

hs.hotkey.bind({}, "F15", function()
    appLauncher.smartLaunchOrFocus("Emacs")
end
)

hs.alert.show("Config loaded")
require('ext.zoom')
spoon.GlobalMute:bindHotkeys({
  toggle = {{}, "F19"}
})
spoon.GlobalMute:configure({
  enforce_desired_state = true,
  stop_sococo_for_zoom  = true,
  unmute_title = "<---- THEY CAN HEAR YOU -----",
  mute_title = "<-- MUTE",
  -- change_screens = "SCREENNAME1, SCREENNAME2"  -- This will only change the background of the specific screens.  string.find()
})
spoon.GlobalMute._logger.level = 3
