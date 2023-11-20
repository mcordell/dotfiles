--- Allow controlling hammerspoon from CLI
require("hs.ipc")
require("ext.key-map")

hs.urlevent.bind("flopmon", require('ext.flipFlopMonitors'))
hs.urlevent.bind("startWork", require('ext.my-utils').startWork)
hs.urlevent.bind("endWork", require('ext.my-utils').shutDownWork)
hs.urlevent.bind("placeWindows", require('ext.my-utils').movePlaces)

hs.alert.show("Config loaded")
