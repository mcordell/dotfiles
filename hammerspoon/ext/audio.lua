local module = {
  vars = {
    speaker = 'Audioengine HD3',
    headphones = 'USB Audio DAC   '
  }
}

module.setAudioOutput = function(name)
  local device = hs.audiodevice.findDeviceByName(name)
  if device ~= nil then
    device:setDefaultOutputDevice()
  end
end

module.toggleAudioOutputSource = function()
  local current = hs.audiodevice.defaultOutputDevice():name()
  if current == module.vars.speaker then
    module.setAudioOutput(module.vars.headphones)
  elseif current == module.vars.headphones then
    module.setAudioOutput(module.vars.speaker)
  end
end

return module
