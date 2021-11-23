#!/bin/dash
SYSTEM=`uname`
SCRIPTPATH=`pwd -P`
DOT_REPO="https://github.com/mcordell/dotfiles"
# DOT_PUSH allows a different push url, in this case we want push to the same
# location but use ssh to avoid https authing
DOT_PUSH="git@github.com:mcordell/dotfiles.git"
DOT_PATH="$HOME/.dotfiles"

setupPackageManager() {
	case $SYSTEM in
		Darwin*)
		echo "Allo osx"
		setupCommandLineTools
		installBrew
		ensureBrew
		brew tap homebrew/cask-versions
		brew update
;;
		Linux*)
		echo "Allo linux"
		sudo apt install -y software-properties-common
		sudo apt update
	;;
	esac;
}

ensureBrew() {
	if [ -z `which brew | grep -v 'not found'` ]
	then
		echo "Brew not present, aborting."
		exit
	fi;
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
			sudo apt install -y zsh git
		;;
	esac;
}

setupZsh() {
	case $SYSTEM in
		Darwin*)
			ZSH_EXEC=$(brew --prefix)/bin/zsh
			CURR_SHELL=$(dscl . -read /Users/$USER UserShell | cut -f 2 -d " ")
			if [ "$ZSH_EXEC" = "$CURR_SHELL" ]
			then
				echo "Shell already set to $ZSH_EXEC for $USER"
			else
				echo "Setting shell to zsh"
				sudo dscl . -create /Users/$USER UserShell "$ZSH_EXEC"
				echo "Restart terminal and run install-with-zsh"
			fi
	    ;;
			Linux*)
			ZSH_EXEC=$(which zsh)
			CURR_SHELL=$(awk -F: -v user="$USER" '$1 == user {print $NF}' /etc/passwd)
			if [ "$ZSH_EXEC" = "$CURR_SHELL" ]
			then
				echo "Shell already set to $ZSH_EXEC for $USER"
			else
				echo "Setting shell to zsh"
				chsh -s "$(which zsh)"
				echo "Restart terminal and run install-with-zsh"
			fi
		;;
	esac;
}

setupDotfiles() {
	git clone --recursive $DOT_REPO $DOT_PATH && cd $DOT_PATH
	git remote set-url --push origin $DOT_PUSH
	export DOT_REPO
	export DOT_PATH
	zsh -c "source $DOT_PATH/zsh/dot/dot.sh; dot_main set"
}

setupPackageManager
installEssentials
setupDotfiles
setupZsh
