local cache  = {}
local module = { cache = cache }

module.maximizeWindow = function()
  hs.window.focusedWindow():maximize(0)
end

module.moveLeft = function()
  local win = hs.window.focusedWindow()
  local target = win:screen():previous()
  win:moveToScreen(target, false, false, 0)
  win:maximize(0)
end

module.moveLeft = function()
  local win = hs.window.focusedWindow()
  local target = win:screen():next()
  win:moveToScreen(target, false, false, 0)
  win:maximize(0)
end

module.splitLeft = function()
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
end

module.splitUp = function()
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
end

module.splitDown = function()
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
end

module.splitRight = function()
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
end

module.moveToLowerRight = function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.w = max.w / 2
    f.h = max.h / 2
    f.x = max.x + f.w
    f.y = max.y + f.h

    win:setFrame(f, 0)
end

module.splitMainFocus = function splitMainFocus(leftApp, rightApp)
  leftApp:mainWindow():moveToScreen(leftScreen, false, false, 0)
  rightApp:mainWindow():moveToScreen(leftScreen, false, false, 0)
  leftApp:mainWindow():moveToUnit(hs.geometry.unitrect(0, 0, .5, 1))
  rightApp:mainWindow():moveToUnit(hs.geometry.unitrect(.5, 0, .5, 1))
  leftApp:mainWindow():focus()
  rightApp:mainWindow():raise()
end

module.moveToMainFocus = function (app)
  app:mainWindow():focus()
  app:mainWindow():moveToScreen(leftScreen, false, false, 0)
  app:mainWindow():moveToUnit(hs.geometry.unitrect(0, 0, 1, 1))
end

return module
