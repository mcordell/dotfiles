# Common Home Manager configuration shared across all systems
{ config, pkgs, ... }:

{
  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  home.stateVersion = "25.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Add packages that should be installed to the user environment.
  home.packages = with pkgs; [
    git
    neovim
    fzf
    navi
    ripgrep
    less
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      # ruby/rails
      be = "bundle exec";
      berspec = "nocorrect bundle exec rspec";
      bp = "bundle exec rails_best_practices --format html; open rails_best_practices_output.html";
      randpw = "openssl rand -base64";
      reset_db = "./bin/rails db:drop RAILS_ENV=test; ./bin/rails db:create RAILS_ENV=test; ./bin/rails db:migrate RAILS_ENV=te
st";
      rspec = "nocorrect rspec";

      # git
      clean_branches = "git checkout \${DEFAULT_GIT_BRANCH:=\"master\"} && git recentb | fzf -m --ansi | awk '{ print $3 }' | x
args git branch -D";
      clean_orig = "find . -iregex .*\\.orig -exec rm -rf {} \\;";
      clear_gemlock = "git reset HEAD -- Gemfile.lock; git checkout -- Gemfile.lock; bundle";
      fu = "git log --oneline | fzf | awk '{ print $1 }' | xargs -I{} git commit --fixup={}";
      g = "git";
      gap = "git add -p";
      gb = "git for-each-ref --sort=committerdate refs/heads/ --format='%(refname:short)'";
      gras = "git rebase -i --autosquash";
      lg = "nocorrect git lg";
      wip = "git commit -m \"wip\"";
      gdu = "git diff @{u}";

      # fzf
      preview = "fzf -m --preview 'bat --color \"always\" {}'";
      select_remove = "fzf -m | xargs rm -rf";

      # eza
      l = "eza";
      la = "eza -lbhHigmuSa --time-style=long-iso --git --color-scale";
      li = "eza --icons";
      ll = "eza -lbF --git";
      lld = "eza -lbhHFGmuSa --group-directories-first";
      llm = "eza -lbGF --git --sort=modified";
      llt = "eza -l --git --tree";
      lt = "eza --tree --level=2";
      lx = "eza -lbhHigmuSa@ --time-style=long-iso --git --color-scale";

      # default overrides
      cat = "bat";
      vim = "nvim";
      v = "nvim";
    };

    sessionVariables = {
      FZF_DEFAULT_OPTS = "--bind='ctrl-o:execute(nvim {})+abort'";
      PAGER = "less";
      DOT_REPO = "https://github.com/mcordell/dotfiles";
      DOT_PATH = "$HOME/.dotfiles";
      DOTFILES_DIR = "$HOME/.dotfiles";
      LANG = "en_US.UTF-8";
      DEFAULT_USER = "michael";
      EDITOR = "nvim";
      BUNDLER_EDITOR = "nvim";
      VISUAL = "nvim";
      LESS = "-F -g -i -M -R -S -w -X -z-4";
      FZF_DEFAULT_COMMAND = "rg --files";
      DIRENV_LOG_FORMAT = "";
    };

    envExtra = ''
      if type brew &>/dev/null; then
        FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
        FPATH=$(brew --prefix)/share/zsh/functions:$FPATH
        autoload -Uz compinit
        compinit
      fi

      # TMPDIR / TMPPREFIX (from .zshenv)
      if [[ -z "$TMPDIR" ]]; then
        export TMPDIR="/tmp/$LOGNAME"
        mkdir -p -m 700 "$TMPDIR"
      fi
      
      TMPPREFIX="''${TMPDIR%/}/zsh"

      # Dotfiles zsh_ruby
      [[ -f "$HOME/.zsh/zsh_ruby" ]] && source "$HOME/.zsh/zsh_ruby"

      # Less input preprocessor (lesspipe)
      if (( $#commands[(i)lesspipe(|.sh)] )); then
        export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
      fi

      # Python virtualenv
      if [[ -n $VIRTUAL_ENV && -e "''${VIRTUAL_ENV}/bin/activate" ]]; then
        source "''${VIRTUAL_ENV}/bin/activate"
      fi

      # pyenv profile
      [[ -f "$HOME/.zsh/pyenv_profile" ]] && source "$HOME/.zsh/pyenv_profile"
    '';

    initContent = ''
      # Powerlevel10k instant prompt (must stay near top of .zshrc)
      setopt EXTENDED_GLOB
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      # PATH must run before instant prompt so brew/commands are available at first prompt
      source ~/.zsh/zsh_path.zsh


      files=(
        "''${HOME}/.zsh/zprezto_init"
        "''${HOME}/.zsh_this_computer"
        "''${HOME}/.zsh/gnupg.zsh"
        "''${HOME}/.zsh/git_keys"
        "''${HOME}/.zsh/zsh_keybindings"
      )
      for f ($^files(.N)) source $f
      unset files

      source ~/.zsh/plugins/forgit/forgit.plugin.zsh

      fpath=($HOME/.zsh/dot $fpath)
      source $HOME/.zsh/dot/dot.sh

      fpath=( "$HOME/.zsh/functions" "''${fpath[@]}" )
      autoload -U $fpath[1]/*(.:t)

      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
      [[ ! -f ~/.oai ]] || source ~/.oai
    '';
  };

  # HM-managed integrations (init added to .zshrc automatically)
  programs.zoxide.enable = true;
  programs.fzf.enable = true;

  programs.navi = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.mise = {
    enable = true;
    enableZshIntegration = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60;
    maxCacheTtl = 120;
    extraConfig = ''
      pinentry-program /opt/homebrew/bin/pinentry-mac
      ttyname $GPG_TTY
    '';
 

  };

  # Configure programs
  # programs.git.enable = true;
  # programs.neovim.enable = true;
}
