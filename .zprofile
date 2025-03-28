#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

[ -f ~/.zsh/pyenv_profile ] && source ~/.zsh/pyenv_profile

eval "$(mise activate zsh --shims)"
