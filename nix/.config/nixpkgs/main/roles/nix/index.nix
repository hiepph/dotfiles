{ config, lib, pkgs, ... }:

{
  home.packages = [
  ];

  programs.zsh = {
    prezto = {
      enable = true;
      prompt.theme = "bart";
    };
  };
}
