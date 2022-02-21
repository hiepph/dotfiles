{ config, lib, pkgs, ... }:

with pkgs;
{
  # refer: https://nixos.wiki/wiki/Zsh
  programs.zsh = {
    prezto = {
      enable = true;
      prompt.theme = "pure";
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

    # build
    cmake

    # languages
    boot # clojure build tool
    babashka # interpreter for Clojure scripting
    (callPackage ./julia {})
    (callPackage ./lua {})
    go
    (callPackage ./gotools {})
    ruby
    nim

    # utils
    (callPackage ./ripgrep {})
    nmap
    bat
    pandoc

    # virtualization
    vagrant

    # db
    postgresql_13

    # backup
    borgbackup
  ];
}
