local module         = {
  vars = {
    comToolsSpace = nil,
    mainLapSpace = nil
  }
}

module.refreshSpaces = function()
  for _, values in pairs(hs.spaces.missionControlSpaceNames()) do
    for id, name in pairs(values) do
      if name == "Desktop 4" then
        module.vars.comToolsSpace = id
      elseif name == "Desktop 1" then
        module.vars.mainLapSpace = id
      end
    end
  end
end

module.refreshSpaces()


return module
