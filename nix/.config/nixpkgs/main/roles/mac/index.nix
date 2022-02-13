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
      "PATH" = "$HOME/scripts:$HOME/go/bin:$PATH";
    };
  };

  home.packages = [
    # text editor
    emacs
    ispell
    indent
    ctags

    # languages
    boot # clojure build tool
    babashka # interpreter for Clojure scripting
    (callPackage ./julia {})
    lua
    go
    ruby

    # utils
    (callPackage ./ripgrep {})
    nmap
    bat
    pandoc
    postgresql_13

    # backup
    borgbackup
  ];
}
