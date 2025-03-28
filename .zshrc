# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
GITSTATUS_LOG_LEVEL=DEBUG
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

setopt EXTENDED_GLOB

source ~/.zsh/zsh_path.zsh

files=(
"${HOME}/.zsh/zprezto_init"
"${HOME}/.zsh/zsh_aliases"
"${HOME}/.zsh_this_computer"
"${HOME}/.zsh/pyenv_zshrc"
"${HOME}/.zsh/gnupg.zsh"
"${HOME}/.zsh/git_keys"
"${HOME}/.zsh/zsh_keybindings"
)
for f ($^files(.N)) source $f
unset files

if [ "$TERM" != "dumb" ]; then
	export ITERM2_SQUELCH_MARK=1
	f="${HOME}/.iterm2_shell_integration.zsh"
	[ -f $f ] && source $f
fi

export MANPAGER="sh -c 'col -bx | bat -l man -p'"

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

if which "fzf" &> /dev/null; then
	source <(fzf --zsh)
fi


[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[[ ! -f ~/.oai ]] || source ~/.oai

eval "$(mise activate zsh)"

