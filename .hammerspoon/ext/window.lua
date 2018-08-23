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

return module
