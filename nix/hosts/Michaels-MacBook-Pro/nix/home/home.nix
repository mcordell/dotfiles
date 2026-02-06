# macOS-specific Home Manager configuration
{ config, pkgs, ... }:

{
  programs.git = {
    settings.user.email = "mike@mikecordell.com";
    signing = {
      key = "0xD54877F05911F2BD";
      signByDefault = true;
    };
  };
}