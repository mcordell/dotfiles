setopt EXTENDED_GLOB
files=(
"${HOME}/.zsh/zprezto_init"
"${HOME}/.zsh/zsh_aliases"
"${HOME}/.fzf.zsh"
"${HOME}/.zsh_this_computer"
"${HOME}/.zsh/pyenv_zshrc"
"${HOME}/.zsh/gnupg.zsh"
"${HOME}/.zsh/git_keys"
"${HOME}/.zsh/zsh_shortcuts"
)
for f ($^files(.N)) source $f
unset files

if [ "$TERM" != "dumb" ]; then
	export ITERM2_SQUELCH_MARK=1
	f="${HOME}/.iterm2_shell_integration.zsh"
	[ -f $f ] && source $f
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

if which "navi" &> /dev/null; then
	eval "$(navi widget zsh)"
fi

eval "$(starship init zsh)"

if which "brew" &> /dev/null; then
	. $(brew --prefix asdf)/libexec/asdf.sh
else
    . $HOME/.asdf/asdf.sh
	fpath=(${ASDF_DIR}/completions $fpath)
	autoload -Uz compinit && compinit
fi

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc"


