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
    go
    goimports

    # utils
    (callPackage ./ripgrep {})
    nmap
    babashka # interpreter for Clojure scripting
    bat
    pandoc

    # backup
    borgbackup
  ];
}
