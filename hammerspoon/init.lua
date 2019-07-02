local highlightWindow = require('ext.drawing').highlightWindow
local template        = require('ext.template')
log             = hs.logger.new('application', 'debug')
local windowTools     = require('ext.window')
local reloadConfig    = function()
  hs.reload()
end
local appLauncher     = require('ext.appLauncher')
local getApp          = function (appName)
  return hs.appfinder.appFromName(appName)
end
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "pad3", windowTools.splitLowerRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "pad9", windowTools.splitUpperRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "pad7", windowTools.splitUpperLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "pad1", windowTools.splitLowerLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "C", function()
    source = [[
        tell application "Google Chrome"
          activate
          set theURL to URL of active tab of front window
          set the clipboard to theURL & return
        end tell
      ]]
    hs.osascript.applescript(source)
end
)
hs.hotkey.bind({"cmd", "ctrl"}, "c", function()
    appLauncher.smartLaunchOrFocus("iTerm2")
                          end
)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "H", windowTools.splitLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "I", windowTools.splitUp)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "L", windowTools.splitRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "M", windowTools.splitDown)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "K", windowTools.maximizeWindow)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", windowTools.moveLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", windowTools.moveRight)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", reloadConfig)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, ".", windowTools.moveToLowerRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "F", function()
    windowTools.splitMainFocus(getApp("Google Chrome"), getApp("iTerm2"))
end)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, ".", windowTools.moveToLowerRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "S", function()
    moveToMainFocus(getApp("Slack"))
end)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "D", function()
    windowTools.splitMainFocus(getApp("Google Chrome"), getApp("Emacs"))
end)

hs.hotkey.bind({}, "F13", function()
    appLauncher.smartLaunchOrFocus("Spotify")
end
)

hs.hotkey.bind({}, "F14", function()
    appLauncher.smartLaunchOrFocus("Google Chrome")
end
)

hs.hotkey.bind({}, "F15", function()
    appLauncher.smartLaunchOrFocus("Emacs")
                          end
)

leftScreen = nil
rightScreen = nil
mainLaptopScreen = nil

xMin = nil

for screen, position in pairs(hs.screen.screenPositions()) do
  if screen:name() == "Color LCD" then
    mainLaptopScreen = screen
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

rightBottom = hs.geometry.unitrect(0.5, 0.5, 0.5, 0.5)
rightTop = hs.geometry.unitrect(0.5, 0, 0.5, 0.5)
leftTop = hs.geometry.unitrect(0, 0, 1, 0.5)
leftBotton = hs.geometry.unitrect(0, 0.5, 1, 0.5)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "N", function()
  local emacs = getApp("Emacs")
  local slack = getApp("Slack")
  local fantastical = getApp("Fantastical")
  local omnifocus = getApp("OmniFocus")
  local spotify = getApp("Spotify")
  layout1 = {
    {emacs, nil, rightScreen, hs.layout.maximized, nil, nil},
    {fantastical, nil, leftScreen, leftTop, nil, nil},
    {omnifocus, nil, leftScreen, leftBotton, nil, nil},
    {spotify, nil, mainLaptopScreen, hs.layout.maximized, nil, nil},
    {slack, nil, mainLaptopScreen, hs.layout.maximized, nil, nil},
  }
  fantastical:mainWindow():raise()
  fantastical:mainWindow():focus()
  hs.eventtap.keyStroke({"cmd"}, "1")
  hs.eventtap.keyStroke({"cmd"}, "t")
  spotify:mainWindow():raise()
  slack:mainWindow():raise()
  emacs:mainWindow():focus()
  omnifocus:mainWindow():raise()
  hs.layout.apply(layout1)
end)

hs.urlevent.bind("flopmon", require('ext.flipFlopMonitors'))

hs.alert.show("Config loaded")
