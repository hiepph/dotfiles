{ config, pkgs, ... }:

let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8
  ];
  python-with-my-packages = pkgs.python38.withPackages my-python-packages;
in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ # tools
      pkgs.git
      pkgs.htop
      pkgs.nmap
      pkgs.tmux
      pkgs.wget

      # text editor
      pkgs.vim
      pkgs.emacs

      # languages
      python-with-my-packages
      pkgs.go
      # pkgs.babashka

      # utils
      pkgs.jq
      pkgs.ripgrep
      pkgs.fzf

      # devops
      pkgs.ansible
      pkgs.sshpass
      pkgs.rclone

      # build
      pkgs.cmake
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
