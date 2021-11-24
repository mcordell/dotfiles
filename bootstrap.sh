#!/bin/bash
SYSTEM=`uname`
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

function installEssentials () {
	general_packages=('golang' 'fasd')

	case $SYSTEM in
		Darwin*)
			specific_packages=(
			'ruby-install' 'chruby'
			'fasd' 'golang')
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

function installFd () {
	case $SYSTEM in
		Darwin*)
			brew install fd
	    ;;
		Linux*)
			echo "Cant install fd from package manager: go here: https://github.com/sharkdp/fd/#on-ubuntu" >> postinstall.log
		;;
	esac
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

function installFzf () {
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	~/.fzf/install
}

function installNeovim () {
	case $SYSTEM in
		Darwin*)
			brew install neovim/neovim/neovim
	    ;;
		Linux*)
			sudo apt-get install neovim
		;;
	esac
	sudo pip3 install neovim
}

function installRipgrep () {
	case $SYSTEM in
		Darwin*)
			brew install ripgrep
	    ;;
		Linux*)
			echo "Cant install ripgrep easily: go here: https://github.com/BurntSushi/ripgrep#installation" >> postinstall.log
		;;
	esac
	sudo pip install neovim
}

function installTldr () {
	case $SYSTEM in
		Darwin*)
			brew install tldr
	    ;;
		Linux*)
			sudo pip install tldr
		;;
	esac
}

function installJq () {
	case $SYSTEM in
		Darwin*)
			brew install jq
	    ;;
		Linux*)
			sudo apt-get install jq
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
installBat
installFancyDiff
installFd
installFzf
installNeovim
installRipgrep
installTldr
installJq
installVisual
setupZsh
setupTmux
setupConfig
vimStuff

#Configure neovim
nvim -c 'autocmd VimEnter * PlugInstall | silent! source $MYVIMRC'

cat postinstall.log
echo " - Copy the theme from $SCRIPTPATH/iterm into iterm"
echo " - Install ruby of choice with: ruby-install ruby #.#.#"
echo " - Install 1Password: https://1password.com/downloads/mac/"
echo " - Sign in to chrome"
echo " - Setup GPG at ~/.gnupg"
echo " - Setup git at ~/.computer_gitconfig"
