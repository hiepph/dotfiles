{ config, lib, pkgs, ... }:

let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8
  ];
  python-with-my-packages = pkgs.python38.withPackages my-python-packages;
in {
  home.packages = [
    # tools
    pkgs.nmap

    # text editor
    pkgs.emacs

    # languages
    python-with-my-packages
    pkgs.go

    # utils
    pkgs.ripgrep
    pkgs.fzf

    # devops
    pkgs.ansible
    pkgs.sshpass
    pkgs.rclone

    # build
    pkgs.cmake
  ];
}
