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
		specific_packages=('the_silver_searcher')
	;;
		Linux*)
		echo 'Allo linux'
		PKG_MANAGER='sudo apt-get'
		specific_packages=('vim-nox' 'silversearcher-ag')
	;;
esac

general_packages=('git' 'zsh' 'tmux')

packages=( "${general_packages[@]}" "${specific_packages[@]}" )

eval $PKG_MANAGER' install '`join ' ' "${packages[@]}"`

rm -rf $HOME/.vim $HOME/.vimrc
ln -s $SCRIPTPATH/.vim $HOME/.vim
ln -s $SCRIPTPATH/.vimrc $HOME/.vimrc

rm -rf $HOME/.zsh*
$SCRIPTPATH/install_zprezto.sh
#ln -s $SCRIPTPATH/.zsh_aliases $HOME/.zsh_aliases
#echo "source $HOME/.zsh_aliases" >> $HOME/.zshrc
rm -rf $HOME/.gitconfig
ln -s $SCRIPTPATH/.gitconfig $HOME/.gitconfig

