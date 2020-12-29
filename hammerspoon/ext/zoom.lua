zoomState = function()
  local zoom = hs.application.get("zoom.us")
  if zoom == nil then
	  return 'not running'
  else
	  if zoom:findMenuItem({"Meeting", "Unmute Audio"}) ~= nil then
		  return 'muted'
	  else
		  return 'unmuted'
	  end
  end
end

unmuteZoom = function()
  local zoom = hs.application.get("zoom.us")
  if zoom ~= nil then
	  zoom:selectMenuItem({"Meeting", "Unmute Audio"})
	  hs.alert.show("Unmuted")
  end
end

muteZoom = function()
  local zoom = hs.application.get("zoom.us")
  if zoom ~= nil then
	  zoom:selectMenuItem({"Meeting", "Mute Audio"})
	  hs.alert.show("Muted")
  end
end

toggleZoomMute = function()
  local c_tbl =
{
  ['muted'] = unmuteZoom,
  ['unmuted'] = muteZoom,
}
  local func = c_tbl[zoomState()]

  if func ~= nil then
	  func()
  end
end

