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
      "PATH" = "$HOME/scripts:$HOME/go/bin:/Library/Tex/texbin:$PATH";
    };

    # shellAliases = {
    # };
  };

  home.packages = [
    # backup
    borgbackup

    # build
    cmake

    # db
    postgresql_13

    # infrastructure
    terraform
    awscli2
    kops
    kubectx
    minikube

    # languages
    boot # clojure build tool
    babashka # interpreter for Clojure scripting
    (callPackage ./julia {})
    (callPackage ./lua {})
    go
    (callPackage ./gotools {})
    ruby
    nim

    # text editor
    emacs
    ispell
    indent
    ctags

    # utils
    (callPackage ./ripgrep {})
    nmap
    bat
    pandoc

    # virtualization
    vagrant
  ];
}
