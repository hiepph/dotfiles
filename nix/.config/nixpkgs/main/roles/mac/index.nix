{ config, lib, pkgs, ... }:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8
  ];
  python-with-my-packages = python38.withPackages my-python-packages;
in {
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
