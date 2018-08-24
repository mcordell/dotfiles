return function ()
	externalMonName = "VA2746 SERIES"
	vertical = nil
	horizontal = nil
	for x, s in pairs(hs.screen.allScreens()) do
		if s:name() == externalMonName then
			if s:rotate() == 0 then
				horizontal = s
			else
				verticalRot = s:rotate()
				vertical = s
			end
		end
	end
	horizontal:rotate(verticalRot)
	vertical:rotate(0)
end
