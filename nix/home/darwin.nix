# macOS/Darwin-specific Home Manager configuration
{ config, pkgs, ... }:

{
  # Add macOS-specific packages
  home.packages = with pkgs; [
    # Add macOS-specific packages here
  ];

  # macOS-only zsh config (merged with default.nix)
  programs.zsh = {
    shellAliases = {
      darwin-build = "(cd ~/.dotfiles && nix run --extra-experimental-features nix-command --extra-experimental-features flakes nix-darwin -- build --flake '.#Michaels-MacBook-Pro')";
      
      desk = "cd ~/Desktop";
      hidehidden = "defaults write com.apple.finder AppleShowAllFiles NO";
      icloud = "cd \"$ICLOUD_DIR\"";
      showhidden = "defaults write com.apple.finder AppleShowAllFiles YES";
    };
    initContent = ''
      if [ "$TERM" != "dumb" ]; then
        export ITERM2_SQUELCH_MARK=1
        f="''${HOME}/.iterm2_shell_integration.zsh"
        [ -f $f ] && source $f
      fi
    '';
  };

  # macOS-only session variables (merged with default.nix; same key here overrides)
  home.sessionVariables = {
    ICLOUD_DIR = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
    # Add other Darwin-only env vars here
    BROWSER = "open";
  };

  # Configure macOS-specific programs
  # programs.brew.enable = true;
}
