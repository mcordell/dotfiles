ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Example aliases
alias zshconfig="subl ~/.zshrc"
alias lt="tree -L 1"
alias lg='nocorrect git lg'
alias rspec='nocorrect rspec'
alias berspec='nocorrect bundle exec rspec'
alias be='bundle exec'
alias bp='bundle exec rails_best_practices --format html; open rails_best_practices_output.html'
alias showhidden='defaults write com.apple.finder AppleShowAllFiles YES'
alias hidehidden='defaults write com.apple.finder AppleShowAllFiles NO'
alias s='subl .'
alias v='vim .'
alias desk='cd ~/Desktop'

#projects
alias of='cd ~/rails_projects/offers'
alias adcon='cd ~/Dropbox/Ruby/adcon-upi-client'
alias vine='cd ~/rails_projects/winestat'
alias blog='cd /Users/michael/Dropbox/blog'

#monk
alias mk='cd ~/monkdev/'
alias mcms='cd ~/monkdev/mcms-vagrant/mcms'
alias mchk='cd ~/monkdev/mchk'
alias mdnt='cd ~/monkdev/mdnt'

#glados
alias sshGlad='ssh 192.168.1.150'

#syncSites


# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rails ruby history)

source $ZSH/oh-my-zsh.sh
source ~/.nvm/nvm.sh

DEFAULT_USER='michael'

# Customize to your needs...
export PATH="/usr/local/bin:/Users/michael/.pear/bin:/Applications/Postgres.app/Contents/MacOS/bin:$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
export EDITOR='vim'
