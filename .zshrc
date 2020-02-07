source "${DOTFILES_DIR}/zsh/zprezto_init"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zsh_this_computer ] && source ~/.zsh_this_computer

export GPG_TTY=`tty`
chruby 2.5.3

if [ "$TERM" != "dumb" ]; then
	test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi
