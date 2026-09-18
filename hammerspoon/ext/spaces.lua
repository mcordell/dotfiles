local module         = {
  vars = {
    comToolsSpace = nil,
    mainLapSpace = nil
  }
}

-- hs.spaces.missionControlSpaceNames() works by opening Mission Control and
-- scraping its Accessibility elements, so it returns nil plus an error message
-- whenever it can't -- notably while init.lua is still being loaded.
module.refreshSpaces = function()
  local ok, names, err = pcall(hs.spaces.missionControlSpaceNames)

  if not ok then
    print("refreshSpaces: missionControlSpaceNames failed: " .. tostring(names))
    return false
  end

  if not names then
    print("refreshSpaces: could not read space names: " .. tostring(err))
    return false
  end

  for _, values in pairs(names) do
    for id, name in pairs(values) do
      if name == "Desktop 4" then
        module.vars.comToolsSpace = id
      elseif name == "Desktop 1" then
        module.vars.mainLapSpace = id
      end
    end
  end

  return true
end

-- Look the spaces up on demand instead of at load: doing it at load both fails
-- and would flash Mission Control on every config reload.
module.ensureSpaces = function()
  if module.vars.comToolsSpace and module.vars.mainLapSpace then
    return true
  end

  return module.refreshSpaces()
end

return module
