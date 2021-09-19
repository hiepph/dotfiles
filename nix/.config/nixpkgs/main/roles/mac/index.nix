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
    shellAliases = {
      "octave" = "/Applications/Octave-6.2.0.app/Contents/Resources/usr/Cellar/octave-octave-app@6.2.0/6.2.0/bin/octave";
    };
  };

  home.packages = [
    # tools
    nmap
    babashka # interpreter for Clojure scripting

    # text editor
    emacs

    # languages
    python-with-my-packages
    go

    # utils
    # (callPackage ./pkgs/ls-colors {})
  ];
}
