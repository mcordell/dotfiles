#!/bin/bash
SYSTEM=`uname`
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

function join { local IFS="$1"; shift; echo "$*"; }

function installBrew () {
	if [ -z `which brew | grep -v 'not found'` ]
	then
		echo 'getting brew'
		ruby \
		-e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" \
		</dev/null
	else
		echo 'We already have brew'
	fi
}

function setupPackageManager () {
	case $SYSTEM in
		Darwin*)
		echo 'Allo osx'
		installBrew
		PKG_MANAGER='brew'
		brew install caskroom/cask/brew-cask
		brew tap caskroom/versions
		brew update
		brew upgrade
		sudo easy_install pip
;;
		Linux*)
		echo 'Allo linux'
		PKG_MANAGER='sudo apt-get'
		sudo apt-get install -y software-properties-common
		sudo add-apt-repository ppa:neovim-ppa/unstable
		sudo apt-get update
	;;
esac
}

function installEssentials () {
	general_packages=('zsh' 'tmux' 'git')

	case $SYSTEM in
		Darwin*)
			specific_packages=('python' 'reattach-to-user-namespace'
			'coreutils' 'gnupg' 'pinentry-mac')
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

function installGUIprograms () {
	case $SYSTEM in
		Darwin*)
			casks=('google-chrome' 'iterm2-nightly' 'alfred')
			brew cask install `join ' ' "${casks[@]}"`
	    ;;
		Linux*)
		;;
	esac
}

function installBat () {
	case $SYSTEM in
		Darwin*)
			brew install bat
	    ;;
		Linux*)
			echo "Cant install bat from package manager: go here: https://github.com/sharkdp/bat#on-ubuntu" >> postinstall.log
		;;
	esac
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
	sudo pip install neovim
}

setupPackageManager
installEssentials
installGUIprograms
installBat
installFancyDiff
installFd
installFzf
installNeovim
# Doing vim stuff
rm -rf $HOME/.vim $HOME/.vimrc
ln -s $SCRIPTPATH/.vim $HOME/.vim
ln -s $SCRIPTPATH/.vimrc $HOME/.vimrc
mkdir $HOME/.config
ln -s $SCRIPTPATH/nvim $HOME/.config/.nvim

# Zsh
#rm -rf $HOME/.zsh*
#$SCRIPTPATH/install_zprezto.sh
ln -s $SCRIPTPATH/.zsh_aliases $HOME/.zsh_aliases
ln -s $SCRIPTPATH/.zsh_osx $HOME/.zsh_osx
ln -s $SCRIPTPATH/.zsh_general $HOME/.zsh_general
echo "source $HOME/.zsh_aliases" >> $HOME/.zshrc
echo "source $HOME/.zsh_osx" >> $HOME/.zshrc
echo "source $HOME/.zsh_general" >> $HOME/.zshrc

# Git
rm -rf $HOME/.gitconfig
ln -s $SCRIPTPATH/.gitconfig $HOME/.gitconfig
ln -s $SCRIPTPATH/.gitignore_global $HOME/.gitignore_global

#Configure neovim
nvim -c 'autocmd VimEnter * PlugInstall | silent! source $MYVIMRC'
