# refer: https://nixos.wiki/wiki/Zsh
{ config, lib, pkgs, ... }:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8
  ];
  python-with-my-packages = python38.withPackages my-python-packages;
in {
  programs.zsh = {
    enable = true;

    prezto = {
      enable = true;
      prompt.theme = "giddie";
    };

    shellAliases = {
      "octave" = "docker run -it --rm gnuoctave/octave:6.3.0 octave";
    };

    initExtra = ''
if [ -n "$\{commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi
'';
  };

  home.packages = [
    # tools
    nmap

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

    # build
    cmake
  ];
}
