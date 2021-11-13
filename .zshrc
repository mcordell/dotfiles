source "${DOTFILES_DIR}/zsh/zprezto_init"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zsh_this_computer ] && source ~/.zsh_this_computer

source "${DOTFILES_DIR}/zsh/gnupg.zsh"

if [ "$TERM" != "dumb" ]; then
	test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

# Add my custom functions
fpath=( "$DOTFILES_DIR/zsh/functions" "${fpath[@]}" )
autoload -U $fpath[1]/*(.:t)
