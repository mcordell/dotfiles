{ pkgs, ... }: {

  home.stateVersion = "22.11";
  home.username = "michael";
  home.homeDirectory = "/home/michael";
  programs.home-manager.enable = true;
  home.packages = [
	  pkgs.cowsay
  ];
}
