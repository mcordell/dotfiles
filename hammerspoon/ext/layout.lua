local module                = {}
local setFantasticalToToday = require('ext.my-utils').setFantasticalToToday
local windowTools           = require('ext.window')
local appTools              = require('ext.appTools')
local getApp                = appTools.getApp
local log                   = hs.logger.new('init', 'debug')


module.refreshScreens     = function()
  module.vars = {
    leftScreen       = nil,
    rightScreen      = nil,
    mainLaptopScreen = nil,
    mainScreen       = nil,
    xMin             = nil,
    dellMonUUID      = nil
  }

  for screen, position in pairs(hs.screen.screenPositions()) do
    if screen:name() == "Color LCD" then
      module.vars.mainLaptopScreen = screen
    elseif screen:name() == "DELL U3818DW" then
      module.vars.mainScreen = screen
      module.vars.dellMonUUID = screen:getUUID()
    elseif module.vars.xMin == nil then
      module.vars.xMin = position.x
      module.vars.leftScreen = screen
    elseif module.vars.xMin < position.x then
      module.vars.rightScreen = screen
    elseif module.vars.xMin > position.x then
      module.vars.rightScreen = module.vars.leftScreen
      module.vars.leftScreen = screen
    end
  end

  if module.vars.mainScreen == nil then
    module.vars.mainScreen = module.vars.mainLaptopScreen
  end
  if module.vars.mainScreen == nil then
    log.df("Main screen %s", "is empty")
  else
    log.df("Main screen %s", module.vars.mainScreen:name())
  end
end

module.tripleSplit        = function(leftAppName, middleAppName, rightApp)
  local left = getApp(leftAppName)
  local mid = getApp(middleAppName)
  local right = getApp(rightApp)

  left:mainWindow():raise()
  mid:mainWindow():raise()
  right:mainWindow():raise()
  mid:mainWindow():focus()
  hs.layout.apply({
    { left,  nil, module.vars.mainScreen, windowTools.cache.leftThird,   nil, nil },
    { mid,   nil, module.vars.mainScreen, windowTools.cache.middleThird, nil, nil },
    { right, nil, module.vars.mainScreen, windowTools.cache.rightThird,  nil, nil },
  })
end

module.splitBrowserTerm   = function()
  module.splitMainFocus(appTools.vars.browserName, appTools.vars.iTerm)
end

module.splitBrowserEditor = function()
  module.splitMainFocus(appTools.vars.browserName, appTools.vars.emacs)
end

module.splitMainFocus     = function(appOne, appTwo)
  windowTools.splitMainFocus(getApp(appOne), getApp(appTwo))
end

module.planning           = function()
  local fantastical = appTools.getFantastical()
  setFantasticalToToday(fantastical)
  module.tripleSplit(appTools.vars.fantastical, appTools.vars.mailmate, appTools.emacs)
end

module.refreshScreens()

return module
