local cache  = {}
local module = { cache = cache }

module.splitDemensions = function()
  local win, f, max, newWidth, increment = module.windowInfo()

  return newWidth
end

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


module.windowInfo = function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local max = win:screen():frame()
  if max.w > 3000 then
    increment = 3
  else
    increment = 2
  end
  newWidth = max.w / increment
  return win, f, max, newWidth, increment
end

module.splitUpperMiddle = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + newWidth
  f.y = max.y
  win:setFrame(f)
end

module.splitLowerMiddle = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + newWidth
  f.y = max.y + half_h
  win:setFrame(f)
end

module.splitUpperLeft = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)
  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end

module.splitLowerLeft = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y + half_h
  win:setFrame(f)
end

module.splitUpperRight = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + (newWidth * (increment - 1))
  f.y = max.y
  win:setFrame(f)
end

module.splitLowerRight = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local half_h = max.h / 2

  f.w = newWidth
  f.h = half_h
  win:setFrame(f, 0)

  f.x = max.x + (newWidth * (increment - 1))
  f.y = max.y + half_h
  win:setFrame(f)
end

module.splitLeft = function()
  local win, f, max, newWidth, increment = module.windowInfo()

  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end

module.splitTwoThirdsLeft = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local newWidth;

  newWidth = (max.w / 3) * 2
  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x
  f.y = max.y
  win:setFrame(f)
end

module.splitRight = function()
  local win, f, max, newWidth, increment = module.windowInfo()

  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + (newWidth * (increment - 1))
  f.y = max.y
  win:setFrame(f)
end

module.splitTwoThirdsRight = function()
  local win, f, max, newWidth, increment = module.windowInfo()
  local newWidth;
  local third = max.w / 3

  newWidth = third * 2
  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + third
  f.y = max.y
  win:setFrame(f)
end

module.splitUp = function()
    local win, f, max, newWidth, increment = module.windowInfo()
	local half = max.h / 2

	f.w = max.w
	f.h = half
	win:setFrame(f, 0)

	f.x = max.x
	f.y = max.y
	win:setFrame(f)
end

module.splitDown = function()
    local win, f, max, newWidth, increment = module.windowInfo()
    local half = max.h / 2
    f.w = max.w
    f.h = half
    win:setFrame(f, 0)

    f.x = max.x
    f.y = max.y + half
    win:setFrame(f)
end

module.splitMiddle = function()
  local win, f, max, newWidth, increment = module.windowInfo()

  f.w = newWidth
  f.h = max.h
  win:setFrame(f, 0)

  f.x = max.x + newWidth
  f.y = max.y
  win:setFrame(f)
end

module.splitTriple = function (leftApp, midApp, rightApp)
  leftApp:mainWindow():moveToUnit(hs.geometry.unitrect(0, 0, .333, 1))
  midApp:mainWindow():moveToUnit(hs.geometry.unitrect(.333, 0, .333, 1))
  rightApp:mainWindow():moveToUnit(hs.geometry.unitrect(.666, 0, .333, 1))
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
