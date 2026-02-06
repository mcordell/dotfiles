# Common Home Manager configuration shared across all systems
{ config, pkgs, ... }:

let
  # Fetch forgit - interactive git commands with fzf
  forgit = pkgs.fetchFromGitHub {
    owner = "wfxr";
    repo = "forgit";
    rev = "53da496336305b8896e9cc2c8c5fbb016f31847c";  # v24.12.0
    sha256 = "sha256-ERVSEmFfjkcmNML8SVjDnvZYxARaukiie2/lnKHJy58=";
  };
in
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
    less
    sqlite  # Required for zsh-histdb
  ];

  # Place zsh plugins in .zsh/plugins directory
  # Sourced directly in initContent since nix-managed prezto runs from nix store
  home.file.".zsh/plugins/forgit".source = forgit;

  # Powerlevel10k configuration
  home.file.".zsh/functions".source = ./../../zsh/functions;
  home.file.".zsh/zsh_path.zsh".source = ./../../zsh/zsh_path.zsh;

  programs.zsh = {
    enable = true;
    enableCompletion = true;

    shellAliases = {
      # ruby/rails
      be = "bundle exec";
      berspec = "nocorrect bundle exec rspec";
      bp = "bundle exec rails_best_practices --format html; open rails_best_practices_output.html";
      randpw = "openssl rand -base64";
      reset_db = "./bin/rails db:drop RAILS_ENV=test; ./bin/rails db:create RAILS_ENV=test; ./bin/rails db:migrate RAILS_ENV=test";
      rspec = "nocorrect rspec";

      # git
      clean_branches = "git checkout \${DEFAULT_GIT_BRANCH:=\"master\"} && git recentb | fzf -m --ansi | awk '{ print $3 }' | xargs git branch -D";
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
        "''${HOME}/.zsh_this_computer"
        "''${HOME}/.zsh/gnupg.zsh"
        "''${HOME}/.zsh/git_keys"
        "''${HOME}/.zsh/zsh_keybindings"
      )
      for f ($^files(.N)) source $f
      unset files

      # Forgit - interactive git commands with fzf (https://github.com/wfxr/forgit)
      # Add completions to fpath before sourcing (for git forgit tab completion)
      fpath=(~/.zsh/plugins/forgit/completions $fpath)
      source ~/.zsh/plugins/forgit/forgit.plugin.zsh

      fpath=( "$HOME/.zsh/functions" "''${fpath[@]}" )
      autoload -U $fpath[1]/*(.:t)

      # zsh-histdb configuration (from https://github.com/larkery/zsh-histdb)
      # Required on macOS - must be set before sourcing
      HISTDB_TABULATE_CMD=(sed -e $'s/\x1f/\t/g')

      # Source zsh-histdb from nix-managed location
      source ~/.zsh/plugins/zsh-histdb/sqlite-history.zsh
      autoload -Uz add-zsh-hook

      # Custom autosuggest strategy using histdb
      _zsh_autosuggest_strategy_histdb_top() {
          local query="
              select commands.argv from history
              left join commands on history.command_id = commands.rowid
              left join places on history.place_id = places.rowid
              where commands.argv LIKE '$(sql_escape $1)%'
              group by commands.argv, places.dir
              order by places.dir != '$(sql_escape $PWD)', count(*) desc
              limit 1
          "
          suggestion=$(_histdb_query "$query")
      }
      ZSH_AUTOSUGGEST_STRATEGY=histdb_top

      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
      [[ ! -f ~/.oai ]] || source ~/.oai
    '';
  };

  programs.git = {
    enable = true;

    # Global gitignore patterns
    ignores = [
      "rails_best_practices_output.html"
      ".rubocop.yml"
      ".DS_Store"
      "dump.rdb"
      "*.sublime-workspace"
      "*.sublime-project"
      "*.swp"
      ".#*"
      "codeception.yml"
      ".projections.json"
      ".dir-locals.el"
      ".registries.yml"
      ".licenses/"
      "zeus.json"
      "custom_plan.rb"
      ".gemrelease"
      ".rubocop-*"
      ".stfolder"
      ".tool-versions"
      ".envrc"
      ".stignore"
    ];

    # # Include computer-specific config
    # includes = [
    #   { path = "~/.computer_gitconfig"; }
    # ];

    # Enable Git LFS
    lfs.enable = true;

    settings = {
      # User configuration
      user = {
        name = "Michael Cordell";
        email = "mike@mikecordell.com";
      };

      # Core settings
      core = {
        editor = "nvim";
        autocrlf = "input";
      };

      # Branch, tag, and init
      init.defaultBranch = "master";
      branch.sort = "committerdate";
      tag.sort = "version:refname";

      # Pull, push, and fetch
      pull.rebase = true;
      push = {
        default = "current";
        autoSetupRemote = true;
        followTags = true;
      };
      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
      };

      # Rebase settings
      rebase = {
        autosquash = true;
        autostash = true;
        updateRefs = true;
      };

      # Rerere (reuse recorded resolution)
      rerere = {
        enabled = true;
        autoupdate = true;
      };

      # UI settings
      color.ui = true;
      column.ui = "auto";

      # Diff settings
      diff = {
        colorMoved = "zebra";
        algorithm = "histogram";
        mnemoicPrefix = true;
        renames = true;
      };

      # Merge settings
      merge = {
        tool = "fugitive";
        conflictstyle = "zdiff3";
      };
      mergetool.fugitive.cmd = "nvim -f --cmd \"let g:fugitivediff=1\" -c \"Gvdiffsplit!\" \"$MERGED\"";

      # Third party integrations
      github.user = "mcordell";
      hub.protocol = "ssh";

      # Aliases
      alias = {
        lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
        ir = "rebase -i --autosquash @{u}";
        a = "add";
        c = "commit";
        s = "status --short";
        please = "push --force-with-lease";
        cam = "commit --amend --no-edit";
        rh = "reset HEAD";
        cob = "checkout -b";
        co = "checkout";
        staged = "diff --cached";
        rc = "rebase --continue";
        dtc = "diff --name-only HEAD HEAD~1";
        df = "diff --name-only";
        compare = "log --left-right --graph --cherry-pick --oneline --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --no-merges";
        g = "grep --break --heading --line-number";
        uncommit = "reset --soft HEAD~1";
        ctags = "!.git/hooks/ctags";
        soft = "reset --soft HEAD~1";
        hard = "reset --hard";
        cleanup = "clean -f **/*orig";
        branchr = "branch --sort=committerdate";
        basebranch = "!gh pr view --json baseRefName -q .baseRefName";
        files = "!git diff --name-only $(git merge-base HEAD \"$MERGE_BASE\")";
        stat = "!git diff --stat $(git merge-base HEAD \"$MERGE_BASE\")";
        review = "!nvim -p $(git files) +\"tabdo Gdiff $MERGE_BASE\" +\"let g:gitgutter_diff_base = '$MERGE_BASE'\"";
        recentb = "!zsh_function_wrapper recent_branches";
      };
    };
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;

    options = {
      features = "decorations side-by-side";
      dark = true;
      navigate = true;
      side-by-side = true;
      decorations = {
        commit-decoration-style = "blue ol";
        commit-style = "raw";
        file-style = "omit";
        hunk-header-decoration-style = "blue box";
        hunk-header-file-style = "red";
        hunk-header-line-number-style = "#067a00";
        hunk-header-style = "file line-number syntax";
      };
      interactive = {
        keep-plus-minus-markers = false;
      };
    };
  };

  # HM-managed integrations (init added to .zshrc automatically)
  programs.zoxide.enable = true;

  programs.navi = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      cheats = {
        paths = [
          "/Users/michael/code/cheatsheets/navi-cheats"
          "/Users/michael/.dotfiles/navi/cheats"
        ];
      };
    };
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

  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [
      batdiff
    ];
    config = {
      theme = "Nord";
    };
  };

  programs.pay-respects = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.ripgrep.enable = true;
  programs.fd.enable = true;
  programs.pandoc.enable = true;
}
