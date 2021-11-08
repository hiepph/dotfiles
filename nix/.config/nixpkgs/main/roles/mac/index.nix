{ config, lib, pkgs, ... }:

with pkgs;
{
  # refer: https://nixos.wiki/wiki/Zsh
  programs.zsh = {
    prezto = {
      enable = true;
      prompt.theme = "pure";
    };

    shellAliases = {
      "octave" = "/Applications/Octave-6.2.0.app/Contents/Resources/usr/Cellar/octave-octave-app@6.2.0/6.2.0/bin/octave";
      "LTspice" = "/Applications/LTspice.app/Contents/MacOS/LTspice";
    };

    sessionVariables = {
      "EDITOR" = "vim";
      "VISUAL" = "vim";
      "PATH" = "$HOME/scripts:$PATH";
    };
  };

  home.packages = [
    # text editor
    emacs
    ispell
    indent

    # languages
    leiningen
    (callPackage ./julia {})

    # utils
    nmap
    babashka # interpreter for Clojure scripting
    bat
    pandoc

    # backup
    borgbackup
  ];
}
