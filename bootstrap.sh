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
			echo 'getting bre'
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
		specific_packages=('the_silver_searcher' 'python')
		sudo easy_install pip
		sudo pip2 install neovim
		casks=('karabiner' 'seil' 'google-chrome' 'iterm2-nightly' 'alfred')
		brew cask install `join ' ' "${casks[@]}"`
;;
		Linux*)
		echo 'Allo linux'
		PKG_MANAGER='sudo apt-get'
		specific_packages=('silversearcher-ag')
		sudo apt-get install -y software-properties-common
		sudo add-apt-repository ppa:neovim-ppa/unstable
		sudo apt-get update
		sudo apt-get install -y neovim python-pip
		sudo pip2 install neovim
	;;
esac

general_packages=('git' 'zsh' 'tmux')

packages=( "${general_packages[@]}" "${specific_packages[@]}" )

eval $PKG_MANAGER' install '`join ' ' "${packages[@]}"`

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
