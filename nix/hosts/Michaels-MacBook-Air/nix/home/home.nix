# macOS-specific Home Manager configuration
{ config, pkgs, ... }:
let
  # Fetch zsh-histdb plugin
  zsh-histdb = pkgs.fetchFromGitHub {
    owner = "larkery";
    repo = "zsh-histdb";
    rev = "90a6c104d0fcc0410d665e148fa7da28c49684eb";
    sha256 = "sha256-vtG1poaRVbfb/wKPChk1WpPgDq+7udLqLfYfLqap4Vg=";
  };
in

{
  programs.git = {
    settings.user.email = "mike@mikecordell.com";
    signing = {
      key = "0xD54877F05911F2BD";
      signByDefault = true;
    };
  };
  home.file.".zsh/plugins/zsh-histdb".source = zsh-histdb;
  home.file.".p10k.zsh".source = ./../../../../../zsh/.p10k.zsh;
  programs.zsh = {
    initContent = ''
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
    '';
    # Prezto configuration - replaces .zpreztorc
    prezto = {
      enable = true;

      # Color output (auto set to 'no' on dumb terminals)
      color = true;

      # Prezto modules to load (order matters)
      pmodules = [
        "environment"
        "terminal"
        "editor"
        "history"
        "directory"
        "spectrum"
        "utility"
        "completion"
        "git"
        "homebrew"
        "history-substring-search"
        "autosuggestions"
        "osx"
        "rsync"
        "prompt"
      ];

      # Editor module configuration
      editor = {
        keymap = "vi";
        promptContext = true;
      };

      # Git module configuration
      git = {
        submoduleIgnore = "all";
      };

      # Prompt module configuration
      prompt = {
        theme = "powerlevel10k";
      };

      # SSH module configuration
      ssh = {
        identities = [
          "id_rsa"
          "id_rsa2"
          "id_github"
        ];
      };

      # Additional zstyle configurations not covered by home-manager options
      extraConfig = ''
        # Autosuggestions color configuration
        zstyle ':prezto:module:autosuggestions' color 'yes'
        zstyle ':prezto:module:autosuggestions:color' found 'fg=10'

        # OS X module - Dash.app man pages keyword
        zstyle ':prezto:module:osx:man' dash-keyword 'manpages'
      '';
    };
  };

  programs.pandoc.enable = true;

  programs.claude-code = {
    enable = true;
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
}
