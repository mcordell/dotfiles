local braveBrowser    = "Brave"
local module          = {
  vars = {
    brave = braveBrowser,
    browserName = braveBrowser,
    fantastical = "Fantastical",
    mailmate = "MailMate",
    teams = "Microsoft Teams",
    emacs = "Emacs",
    zoom = "zoom.us",
    slack = "Slack",
    outlook = "Microsoft Outlook"
  }
}
local log             = hs.logger.new('init', 'debug')

module.getFantastical = function()
  return module.getApp(module.vars.fantastical)
end

module.getTeams       = function()
  return module.getApp(module.vars.teams)
end

module.getZoom        = function()
  return module.getApp(module.vars.zoom)
end

module.getApp         = function(appName)
  local app = hs.application.find(appName)
  if app == nil then
    log.df("App is nil %s", "nil")
    app = hs.application.open(appName, 1, true)
    log.df("App is nil %s", app)
    return app
  else
    return app
  end
end

return module
