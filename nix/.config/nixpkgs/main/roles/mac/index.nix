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
    enable = true;

    prezto = {
      enable = true;
      prompt.theme = "giddie";
    };

    shellAliases = {
    };

    initExtra = ''
# fzf binding keys
if [ -n "$\{commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

# enable direnv
eval "$(direnv hook zsh)"
'';
  };

  home.packages = [
    # nix essential
    direnv # https://direnv.net/

    # tools
    nmap
    babashka # interpreter for Clojure scripting

    # text editor
    emacs

    # languages
    python-with-my-packages
    go

    # utils
    ripgrep
    fzf
    # (callPackage ./pkgs/ls-colors {})

    # devops
    ansible
    sshpass
    rclone
  ];
}
