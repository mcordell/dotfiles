local cache = {
  bindings    = {},
  launchTimer = nil
}

local module = { cache = cache }

-- force application launch or focus
module.forceLaunchOrFocus = function(appName)
  local appInstance  = hs.application.get(appName)
  local isRunning    = appInstance and appInstance:isRunning()
  local focusTimeout = isRunning and 0.1 or 1.5

  -- first focus/launch with hammerspoon
  hs.application.launchOrFocus(appName)

  -- clear timer if exists
  if cache.launchTimer then cache.launchTimer:stop() end

  -- wait for window to appear and try hard to show the window
  cache.launchTimer = hs.timer.doAfter(focusTimeout, function()
    local frontmostApp     = hs.application.frontmostApplication()
    local frontmostWindows = hs.fnutils.filter(frontmostApp:allWindows(), function(win) return win:isStandard() end)

    -- break if this app is not frontmost (when/why?)
    if frontmostApp:title() ~= appName then
      log.d('Expected app in front: ' .. appName .. ' got: ' .. frontmostApp:title())
      return
    end

    if #frontmostWindows == 0 then
      -- check if there's app name in window menu (Calendar, Messages, etc...)
      if frontmostApp:findMenuItem({ 'Window', appName }) then
        -- select it, usually moves to space with this window
        frontmostApp:selectMenuItem({ 'Window', appName })
      else
        -- otherwise send cmd-n to create new window
        hs.eventtap.keyStroke({ 'cmd' }, 'n')
      end
    end
  end)
end

-- smart app launch or focus or cycle windows
module.smartLaunchOrFocus = function(launchApps)
  local frontmostWindow = hs.window.frontmostWindow()
  local runningApps     = hs.application.runningApplications()
  local runningWindows  = {}
  local target          = hs.window.focusedWindow():screen()

  launchApps = type(launchApps) == 'table' and launchApps or { launchApps }

  -- filter running applications by apps array
  local runningApps = hs.fnutils.map(launchApps, function(launchApp)
    return hs.application.get(launchApp)
  end)

  -- create table of sorted windows per application
  hs.fnutils.each(runningApps, function(runningApp)
    local standardWindows = hs.fnutils.filter(runningApp:allWindows(), function(win)
      return win:isStandard()
    end)

    -- sort by id, so windows don't jump randomly every time
    table.sort(standardWindows, function(a, b) return a:id() > b:id() end)

    -- concat with all running windows
    hs.fnutils.concat(runningWindows, standardWindows);
  end)

  if #runningApps == 0 then
    -- if no apps are running then launch first one in list
    module.forceLaunchOrFocus(launchApps[1])
  elseif #runningWindows == 0 then
    -- if some apps are running, but no windows - force create one
    module.forceLaunchOrFocus(runningApps[1]:title())
  else
    -- check if one of windows is already focused
    local currentIndex = hs.fnutils.indexOf(runningWindows, frontmostWindow)
    local win

    if not currentIndex then
      -- if none of them is selected focus the first one
      win = runningWindows[1]
    else
      -- otherwise cycle through all the windows
      local newIndex = currentIndex + 1
      if newIndex > #runningWindows then newIndex = 1 end
      win = runningWindows[newIndex]
    end

    win:moveToScreen(target, false, false, 0)
    win:raise():focus()
    highlightWindow()
  end
end

module.launcher = function(name)
  local win = hs.window.focusedWindow()
  local target = win:screen()
  local appInstance  = hs.application.get(name)
  local isRunning    = appInstance and appInstance:isRunning()
  local focusTimeout = isRunning and 0.1 or 1.5

  hs.application.launchOrFocus(name)
  hs.application.enableSpotlightForNameSearches(true)

  -- clear timer if exists
  if module.cache.launchTimer then module.cache.launchTimer:stop() end

  -- wait for window to appear and try hard to show the window
  module.cache.launchTimer = hs.timer.doAfter(focusTimeout, function()
                                                local appWin = hs.window.focusedWindow()
                                                appWin:moveToScreen(target, false, false, 0)
                                                appWin:maximize(0)
                                                            end
  )
end

return module
