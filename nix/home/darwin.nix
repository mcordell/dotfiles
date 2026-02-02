# macOS/Darwin-specific Home Manager configuration
{ config, pkgs, ... }:

{
  # Add macOS-specific packages
  home.packages = with pkgs; [
    # Add macOS-specific packages here
  ];

  # macOS-only session variables (merged with default.nix; same key here overrides)
  home.sessionVariables = {
    ICLOUD_DIR = "$HOME/Library/Mobile Documents/com~apple~CloudDocs";
    # Add other Darwin-only env vars here
    BROWSER = "open";
  };

  # iTerm2 shell integration (Darwin only; appended to .zshrc)
  programs.zsh.initExtra = ''
    if [ "$TERM" != "dumb" ]; then
      export ITERM2_SQUELCH_MARK=1
      f="''${HOME}/.iterm2_shell_integration.zsh"
      [ -f $f ] && source $f
    fi
  '';

  # Configure macOS-specific programs
  # programs.brew.enable = true;
}
