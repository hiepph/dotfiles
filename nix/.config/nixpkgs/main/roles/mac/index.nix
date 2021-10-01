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
    };

    sessionVariables = {
      "PATH" = "$HOME/scripts:$PATH";
      "BABASHKA_PRELOADS" = "(require '[babashka.curl :as curl]) (require '[cheshire.core :as json])";
    };
  };

  home.packages = [
    # text editor
    emacs
    ispell
    indent

    # languages

    # utils
    nmap
    babashka # interpreter for Clojure scripting
    bat
  ];
}
