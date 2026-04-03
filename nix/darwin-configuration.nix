# macOS system configuration managed by nix-darwin
{
  config,
  pkgs,
  lib,
  inputs,
  hostname,
  user,
  ...
}:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # Add system-wide packages here
  ];

  system.primaryUser = "michael";

  homebrew = {
    enable = true;
    casks = [
      "1password"
      "1password-cli"
      "alfred"
      "brave-browser"
      "claude"
      "hammerspoon"
      "iterm2"
      "karabiner-elements"
      "mailmate@beta"
      "proton-mail-bridge"
      "tailscale-app"
    ];
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Enable nix flakes
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Configure system defaults
  system.defaults.dock.autohide = true;
  # system.defaults.finder.AppleShowAllFiles = true;
  system.defaults.NSGlobalDomain = {
    InitialKeyRepeat = 12;
    KeyRepeat = 1;
  };
}
