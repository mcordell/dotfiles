#!/usr/bin/env bash
# Sets dock to a minimal set of apps

defaults write com.apple.dock persistent-apps -array \
  '<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>file:///System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/</string><key>_CFURLStringType</key><integer>15</integer></dict></dict></dict>' \
  '<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>file:///System/Applications/Messages.app/</string><key>_CFURLStringType</key><integer>15</integer></dict></dict></dict>' \
  '<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>file:///System/Applications/Reminders.app/</string><key>_CFURLStringType</key><integer>15</integer></dict></dict></dict>' \
  '<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>file:///System/Applications/iPhone Mirroring.app/</string><key>_CFURLStringType</key><integer>15</integer></dict></dict></dict>'

killall Dock
