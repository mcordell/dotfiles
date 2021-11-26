#!/bin/bash
SYSTEM=`uname`
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

function installEssentials () {
	general_packages=('golang')

	case $SYSTEM in
		Darwin*)
			specific_packages=(
			'ruby-install' 'chruby'
			'golang')
			packages=( "${general_packages[@]}" "${specific_packages[@]}" )
			eval $PKG_MANAGER' install '`join ' ' "${packages[@]}"`
	    ;;
			Linux*)
			specific_packages=('python-pip')
			packages=( "${general_packages[@]}" "${specific_packages[@]}" )
			eval $PKG_MANAGER' install -y '`join ' ' "${packages[@]}"`
		;;
	esac
}

function setupConfig() {
	mkdir $HOME/.config
	ln -s "$SCRIPTPATH/config/wtf" $HOME/.config/wtf
}

function setupTmux() {
	ln -s "$SCRIPTPATH/.tmux.conf" $HOME/.tmux.conf
	pip3 install powerline-status
}

installEssentials
setupTmux
setupConfig

cat postinstall.log
echo " - Copy the theme from $SCRIPTPATH/iterm into iterm"
echo " - Install ruby of choice with: ruby-install ruby #.#.#"
echo " - Install 1Password: https://1password.com/downloads/mac/"
echo " - Sign in to chrome"
echo " - Setup GPG at ~/.gnupg"
echo " - Setup git at ~/.computer_gitconfig"
