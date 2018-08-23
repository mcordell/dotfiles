local highlightWindow = require('ext.drawing').highlightWindow
local template        = require('ext.template')
local log             = hs.logger.new('application', 'debug')

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()
  local half = max.w / 2
  f.w = half
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "I", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()
    local half = max.h / 2
    f.w = max.w
    f.h = half
    win:setFrame(f, 0)

    f.x = max.x
    f.y = max.y
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "M", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()
    local half = max.h / 2
    f.w = max.w
    f.h = half
    win:setFrame(f, 0)

    f.x = max.x
    f.y = max.y + half
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "K", function()
  hs.window.focusedWindow():maximize(0)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
  local win = hs.window.focusedWindow()
  local target = win:screen():previous()
  win:moveToScreen(target, false, false, 0)
  win:maximize(0)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
  local win = hs.window.focusedWindow()
  local target = win:screen():next()
  win:moveToScreen(target, false, false, 0)
  win:maximize(0)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()
  local half = max.w / 2

  f.w = half
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + half
  f.y = max.y
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
  hs.reload()
end)
hs.alert.show("Config loaded")


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

function getApp(appName)
  return hs.appfinder.appFromName(appName)
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

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "S", function()
    moveToMainFocus(getApp("Slack"))
end)


hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "D", function()
    splitMainFocus(getApp("Google Chrome"), getApp("Emacs"))
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "F", function()
    splitMainFocus(getApp("Google Chrome"), getApp("iTerm2"))
end)

function moveToMainFocus(app)
  app:mainWindow():focus()
  app:mainWindow():moveToScreen(leftScreen, false, false, 0)
  app:mainWindow():moveToUnit(hs.geometry.unitrect(0, 0, 1, 1))
end

function splitMainFocus(leftApp, rightApp)
  leftApp:mainWindow():moveToScreen(leftScreen, false, false, 0)
  rightApp:mainWindow():moveToScreen(leftScreen, false, false, 0)
  leftApp:mainWindow():moveToUnit(hs.geometry.unitrect(0, 0, .5, 1))
  rightApp:mainWindow():moveToUnit(hs.geometry.unitrect(.5, 0, .5, 1))
  leftApp:mainWindow():focus()
  rightApp:mainWindow():raise()
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, ".", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.w = max.w / 2
    f.h = max.h / 2
    f.x = max.x + f.w
    f.y = max.y + f.h

    win:setFrame(f, 0)
end)
