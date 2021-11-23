source ~/.zsh/zprezto_init

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zsh_this_computer ] && source ~/.zsh_this_computer

source ~/.zsh/gnupg.zsh

if [ "$TERM" != "dumb" ]; then
	test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

source ~/.zsh/plugins/forgit/forgit.plugin.zsh

fpath=($DOTFILES_DIR/zsh/dot $fpath)  # <- for completion
source $HOME/zsh/dot/dot.sh

# Add my custom functions
fpath=( "$HOME/.zsh/functions" "${fpath[@]}" )
autoload -U $fpath[1]/*(.:t)
