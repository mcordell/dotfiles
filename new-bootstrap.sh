#!/bin/dash
SYSTEM=`uname`
SCRIPTPATH=`pwd -P`


while true; do
    read -p "Before continuing has iCloud downloaded all of the dotfiles dir [y/n]?" yn
    case $yn in
        [Yy]* ) break;;
		[Nn]* ) echo "Download it dummy;"; open "$(dirname "$SCRIPTPATH")"; exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

setupPackageManager() {
	case $SYSTEM in
		Darwin*)
		echo "Allo osx"
		setupCommandLineTools
		installBrew
		brew tap homebrew/cask-versions
		brew update
;;
		Linux*)
		echo "Allo linux"
		sudo apt-get install -y software-properties-common
		sudo apt-get update
	;;
	esac;
}

setupCommandLineTools() {
	if xcode-select -p 1>/dev/null
	then
		echo "Xcode command line installed"
	else
		echo "Installing Xcode command line tools"
		xcode-select --install
	fi;
}

installBrew() {
	if [ -z `which brew | grep -v 'not found'` ]
	then
		echo "getting brew"
		ruby \
		-e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" \
		</dev/null
	else
		echo "We already have brew"
	fi;
}

installEssentials() {
	case $SYSTEM in
		Darwin*)
			brew install zsh git
	    ;;
			Linux*)
			sudo apt-get install -y zsh git
		;;
	esac;
}

setupZsh() {
	echo "Changing shell to zsh"
	case $SYSTEM in
		Darwin*)
			zsh_path=$(brew --prefix)/bin/zsh
			sudo dscl . -create /Users/$USER UserShell "$zsh_path"
	    ;;
			Linux*)
			sudo chsh -s "$(which zsh)"
		;;
	esac;
	echo "Restart terminal and run install-with-zsh"
}

setupPackageManager
installEssentials
setupZsh
