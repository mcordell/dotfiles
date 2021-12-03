dotfiles
========

My Dot Files.

Current active setup (late 2021):

- Editors: neovim and emacs with doom-emacs
- macOS: karabiner elements, hammerspoon
- shell and terminal: zsh with prezto and customization

## Setup

This repo has a bootstrap script that setups package mangement, zsh, git,
and this repo on a new macOS or Ubuntu/debian machine.

1. `wget https://raw.githubusercontent.com/mcordell/dotfiles/master/bootstrap.sh;
chmod +x bootstrap.sh; ./bootstrap.sh`
2. `After bootstrapping runs, and zsh is the main shell, return to .dotfiles
directory and run ./install_with_zsh`
