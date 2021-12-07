export BROWSER='open'
export PAGER='less'
SCRIPTPATH=$(readlink $HOME/.zshenv)
export DOT_REPO="https://github.com/mcordell/dotfiles"
export DOT_PATH="$HOME/.dotfiles"
export DOTFILES_DIR=$DOT_PATH
export ICLOUD_DIR="$HOME/Library/Mobile Documents/com~apple~CloudDocs"

source ~/.zsh/zsh_path

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
  FPATH=$(brew --prefix)/share/zsh/functions:$FPATH
  autoload -Uz compinit
  compinit
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion


#
# Language
#
if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

#
# Temporary Files
#
if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"

source ~/.zsh/zsh_ruby
# Editors
export DEFAULT_USER='michael'
export EDITOR='nvim'
export BUNDLER_EDITOR='nvim'
export VISUAL='nvim'


export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# Programs

# Less
#
# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# Use ripgrep to find files for fzf (respects gitignore)
export FZF_DEFAULT_COMMAND='rg --files'
