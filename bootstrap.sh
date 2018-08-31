#!/bin/bash
SYSTEM=`uname`
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`

popd > /dev/null
echo $SYSTEM

function join { local IFS="$1"; shift; echo "$*"; }

case $SYSTEM in
	Darwin*)
		echo 'Allo osx'
		if [ -z `which brew | grep -v 'not found'` ]
		then
			echo 'getting brew'
			ruby \
			-e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" \
			</dev/null
		else
			echo 'weve got brew'
		fi

		PKG_MANAGER='brew'
		brew install caskroom/cask/brew-cask
		brew tap caskroom/versions
		brew install neovim/neovim/neovim
		brew update
		brew upgrade
		specific_packages=('python' 'ripgrep')
		sudo easy_install pip
		sudo pip install neovim
		casks=('google-chrome' 'iterm2-nightly' 'alfred')
		brew cask install `join ' ' "${casks[@]}"`
;;
		Linux*)
		echo 'Allo linux'
		PKG_MANAGER='sudo apt-get'
		specific_packages=()
		sudo apt-get install -y software-properties-common
		sudo add-apt-repository ppa:neovim-ppa/unstable
		sudo apt-get update
		sudo apt-get install -y neovim python-pip
		sudo pip2 install neovim
		echo "Cant install ripgrep easily: go here: https://github.com/BurntSushi/ripgrep#installation"
	;;
esac

general_packages=('git' 'zsh' 'tmux')

packages=( "${general_packages[@]}" "${specific_packages[@]}" )

eval $PKG_MANAGER' install '`join ' ' "${packages[@]}"`

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

installBat
installFzf
installFancyDiff
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
