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
    # Add common packages here
  ];

  # Configure programs
  # programs.git.enable = true;
  # programs.neovim.enable = true;
}
