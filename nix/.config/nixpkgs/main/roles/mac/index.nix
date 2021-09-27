{ config, lib, pkgs, ... }:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8
  ];
  python-with-my-packages = python38.withPackages my-python-packages;
in {
  # refer: https://nixos.wiki/wiki/Zsh
  programs.zsh = {
    prezto = {
      enable = true;
      prompt.theme = "giddie";
    };

    shellAliases = {
      "octave" = "/Applications/Octave-6.2.0.app/Contents/Resources/usr/Cellar/octave-octave-app@6.2.0/6.2.0/bin/octave";
    };

    sessionVariables = {
      "PATH" = "$HOME/scripts:$PATH";
    };
  };

  home.packages = [
    # text editor
    emacs
    ispell

    # languages
    python-with-my-packages
    janet
    go

    # utils
    # (callPackage ./pkgs/ls-colors {})
    nmap
    babashka # interpreter for Clojure scripting
    bat
  ];
}
