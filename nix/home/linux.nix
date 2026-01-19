# Linux-specific Home Manager configuration
{ config, pkgs, ... }:

{
  # Add Linux-specific packages
  home.packages = with pkgs; [
    # Add Linux-specific packages here
  ];

  # Configure Linux-specific programs
  # programs.firefox.enable = true;
}
