defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 12
defaults write -g com.apple.swipescrolldirection 0
defaults write com.apple.dock "autohide" -bool "true" && killall Dock
defaults write com.apple.TimeMachine "DoNotOfferNewDisksForBackup" -bool "false"
