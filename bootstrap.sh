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

function installSpacemacs() {
	ln -s $SCRIPTPATH/spacemacs $HOME/.spacemacs
	ln -s $SCRIPTPATH/.spacemacs.d $HOME/.spacemacs.d
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
	cd ~/.emacs.d
	git checkout develop
	git pull
	rm -rf ~/.emacs.d/private
	ln -s $SCRIPTPATH/emacs.d/private ~/.emacs.d/private
}

function installFancyDiff () {
	case $SYSTEM in
		Darwin*)
			brew install diff-so-fancy
	    ;;
		Linux*)
			echo "Trying to install fancy-diff to /usr/local/bin make sure thats in the path" >> postinstall.log
			curl -o /usr/local/bin/diff-so-fancy https://raw.githubusercontent.com/so-fancy/diff-so-fancy/63568e814f7e71b01f137eeb82792efe6ea6a0b9/third_party/build_fatpack/diff-so-fancy
			chmod +x /usr/local/bin/diff-so-fancy
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

setupPackageManager
installEssentials
installGUIprograms
installFancyDiff
installVisual
setupZsh
setupTmux
setupConfig
vimStuff

cat postinstall.log
echo " - Copy the theme from $SCRIPTPATH/iterm into iterm"
echo " - Install ruby of choice with: ruby-install ruby #.#.#"
echo " - Install 1Password: https://1password.com/downloads/mac/"
echo " - Sign in to chrome"
echo " - Setup GPG at ~/.gnupg"
echo " - Setup git at ~/.computer_gitconfig"
