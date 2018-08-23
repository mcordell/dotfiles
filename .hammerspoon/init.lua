local highlightWindow = require('ext.drawing').highlightWindow
local template        = require('ext.template')
local log             = hs.logger.new('application', 'debug')
local windowTools     = require('ext.window')
local reloadConfig    = function()
  hs.reload()
end
local appLauncher     = require('ext.appLauncher')
local getApp          = function (appName)
  return hs.appfinder.appFromName(appName)
end

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "H", windowsTools.splitLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "I", windowsTools.splitUp)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "L", windowTools.splitRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "M", windowsTools.splitDown)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "K", windowTools.maximize)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", windowTools.moveLeft)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", windowTools.moveRight)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", reloadConfig)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, ".", windowTools.moveToLowerRight)
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "F", function()
    windowTool.splitMainFocus(getApp("Google Chrome"), getApp("iTerm2"))
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

xMin = nil

for screen, position in pairs(hs.screen.screenPositions()) do
  if xMin == nil then
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

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "N", function()
  local emacs = getApp("Emacs")
  local chrome = getApp("Google Chrome")
  local emacs = getApp("Emacs")
  local slack = getApp("Slack")
  local iterm = getApp("iTerm2")
  local messages = getApp("Messages")
  layout1 = {
    {emacs, nil, leftScreen, hs.layout.maximized, nil, nil},
    {chrome, nil, rightScreen, hs.layout.left50, nil, nil},
    {slack, nil, rightScreen, rightBottom, nil, nil},
    {iterm, nil, rightScreen, rightTop, nil, nil},
  }
  slack:mainWindow():raise()
  chrome:mainWindow():raise()
  iterm:mainWindow():raise()
  emacs:mainWindow():focus()
  hs.layout.apply(layout1)
end)

hs.alert.show("Config loaded")
