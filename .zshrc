source ~/.zsh/zprezto_init
source ~/.zsh/zsh_aliases

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zsh_this_computer ] && source ~/.zsh_this_computer
[ -f ~/.zsh/pyenv_zshrc ] && source ~/.zsh/pyenv_zshrc

source ~/.zsh/gnupg.zsh

if [ "$TERM" != "dumb" ]; then
	test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

source ~/.zsh/plugins/forgit/forgit.plugin.zsh

fpath=($HOME/.zsh/dot $fpath)  # <- for completion
source $HOME/.zsh/dot/dot.sh

# Add my custom functions
fpath=( "$HOME/.zsh/functions" "${fpath[@]}" )
autoload -U $fpath[1]/*(.:t)


#zoxide (jump tool)
if which "zoxide" &> /dev/null; then
	eval "$(zoxide init zsh)"
fi

if [ -f "/opt/homebrew/opt/asdf/libexec/asdf.sh" ]; then
	. /opt/homebrew/opt/asdf/libexec/asdf.sh
	eval "$(asdf exec direnv hook zsh)"
	direnv() { asdf exec direnv "$@"; }
fi

if [ -f "/usr/local/opt/asdf/libexec/asdf.sh" ]; then
	. /usr/local/opt/asdf/libexec/asdf.sh
fi

eval "$(starship init zsh)"
