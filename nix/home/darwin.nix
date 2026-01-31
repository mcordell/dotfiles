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

  # Configure macOS-specific programs
  # programs.brew.enable = true;
}
