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

module.moveRight = function()
  local win = hs.window.focusedWindow()
  local target = win:screen():next()
  win:moveToScreen(target, false, false, 0)
  win:maximize(0)
end

module.splitLowerLeft = function()
  local win, f, max = module.windowInfo()
  local half_h, half_w = max.h / 2, max.w / 2

  f.w = half_w
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y + half_h
  win:setFrame(f)
end

module.windowInfo = function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local max = win:screen():frame()
  return win, f, max
end

module.splitUpperLeft = function()
  local win, f, max = module.windowInfo()
  local half_h = max.h / 2
  local half_w = max.w / 2

  f.w = half_w
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end

module.splitUpperRight = function()
  local win, f, max = module.windowInfo()
  local half_h = max.h / 2
  local half_w = max.w / 2

  f.w = half_w
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + half_w
  f.y = max.y
  win:setFrame(f)
end

module.splitLowerRight = function()
  local win, f, max = module.windowInfo()
  local half_h = max.h / 2
  local half_w = max.w / 2

  f.w = half_w
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + half_w
  f.y = max.y + half_h
  win:setFrame(f)
end

module.splitLeft = function()
  local win, f, max = module.windowInfo()
  local newWidth;

  if max.w > 2000 then
	newWidth = max.w / 3
  else
	newWidth = max.w / 2
  end

  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end

module.splitUp = function()
    local win, f, max = module.windowInfo()
	local half = max.h / 2

	f.w = max.w
	f.h = half
	win:setFrame(f, 0)

	f.x = max.x
	f.y = max.y
	win:setFrame(f)
end

module.splitDown = function()
    local win, f, max = module.windowInfo()
    local half = max.h / 2
    f.w = max.w
    f.h = half
    win:setFrame(f, 0)

    f.x = max.x
    f.y = max.y + half
    win:setFrame(f)
end

module.splitRight = function()
  local win, f, max = module.windowInfo()
  local newWidth, adder;

  if max.w > 2000 then
	newWidth = max.w / 3
	adder = 2 * newWidth
  else
	newWidth = max.w / 2
	adder = newWidth
  end

  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + adder
  f.y = max.y
  win:setFrame(f)
end

module.splitMiddle = function()
  local win, f, max = module.windowInfo()
  local third = max.w / 3

  f.w = third
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + third
  f.y = max.y
  win:setFrame(f)
end

module.moveToLowerRight = function()
	local win, f, max = module.windowInfo()

    f.w = max.w / 2
    f.h = max.h / 2
    f.x = max.x + f.w
    f.y = max.y + f.h

    win:setFrame(f, 0)
end

module.splitMainFocus = function (leftApp, rightApp)
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
