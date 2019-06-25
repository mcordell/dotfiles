#!/bin/bash
SYSTEM=`uname`
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

while true; do
    read -p "Before continuing has iCloud downloaded all of the dotfiles dir [y/n]?" yn
    case $yn in
        [Yy]* ) echo "ok"; break;;
		[Nn]* ) echo "Download it dummy;"; open "$(dirname "$SCRIPTPATH")"; exit;;
        * ) echo "Please answer yes or no.";;
    esac
done


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
	general_packages=('zsh' 'tmux' 'git' 'golang' 'fasd' 'pandoc')

	case $SYSTEM in
		Darwin*)
			specific_packages=('python' 'reattach-to-user-namespace'
			'coreutils' 'gnupg' 'pinentry-mac' 'wget' 'ruby-install' 'chruby'
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

function installGUIprograms () {
	case $SYSTEM in
		Darwin*)
			brew cask tap d12frosted/emacs-plus
			brew cask install emacs-plus --HEAD
			casks=('google-chrome' 'iterm2-nightly' 'alfred' 'hammerspoon'
			'karabiner-elements' 'spotify' 'slack' 'notion' 'omnifocus'
			'airmail' 'viscosity' '')
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

function vimStuff() {
	# Doing vim stuff
	rm -rf $HOME/.vim $HOME/.vimrc
	ln -s "$SCRIPTPATH/.vim" $HOME/.vim
	ln -s "$SCRIPTPATH/.vimrc" $HOME/.vimrc
	mkdir $HOME/.config
	ln -s "$SCRIPTPATH/nvim" $HOME/.config/.nvim
}

function setupZsh() {
	ln -s "$SCRIPTPATH/.zshrc" $HOME
	ln -s "$SCRIPTPATH/zsh/zsh_aliases" "$HOME/.zsh_aliases"
	ln -s "$SCRIPTPATH/zsh/zshenv" "$HOME/.zshenv"
	ln -s "$SCRIPTPATH/zsh/zpreztorc" "$HOME/.zpreztorc"
	"$SCRIPTPATH/install_zprezto.sh"
}

function setupGit() {
	rm -rf $HOME/.gitconfig
	ln -s "$SCRIPTPATH/.gitconfig" $HOME/.gitconfig
	ln -s "$SCRIPTPATH/.gitignore_global" $HOME/.gitignore_global
}

function installVisual()  {
	# clone
	git clone https://github.com/powerline/fonts.git --depth=1
	# install
	cd fonts
	./install.sh
	# clean-up a bit
	cd ..
	rm -rf fonts
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
setupZsh
setupGit

# Doing vim stuff
rm -rf $HOME/.vim $HOME/.vimrc
ln -s $SCRIPTPATH/.vim $HOME/.vim
ln -s $SCRIPTPATH/.vimrc $HOME/.vimrc
mkdir $HOME/.config
ln -s $SCRIPTPATH/nvim $HOME/.config/.nvim

# Zsh
#$SCRIPTPATH/install_zprezto.sh
ln -s $SCRIPTPATH/.zshrc $HOME/.zshrc
ln -s $SCRIPTPATH/zsh/zsh_aliases $HOME/.zsh_aliases
ln -s $SCRIPTPATH/zsh/zshenv $HOME/.zshenv
ln -s $SCRIPTPATH/zsh/zpreztorc $HOME/.zpreztorc

# Git
rm -rf $HOME/.gitconfig
ln -s $SCRIPTPATH/.gitconfig $HOME/.gitconfig
ln -s $SCRIPTPATH/.gitignore_global $HOME/.gitignore_global

#Configure neovim
nvim -c 'autocmd VimEnter * PlugInstall | silent! source $MYVIMRC'

cat postinstall.log
echo "Copy the theme from $SCRIPTPATH/iterm into iterm"
echo "Install ruby of choice with: ruby-install ruby #.#.#"
