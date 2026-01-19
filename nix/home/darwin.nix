# macOS/Darwin-specific Home Manager configuration
{ config, pkgs, ... }:

{
  # Add macOS-specific packages
  home.packages = with pkgs; [
    # Add macOS-specific packages here
  ];

  # Configure macOS-specific programs
  # programs.brew.enable = true;
}
