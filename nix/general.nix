{ config, lib, pkgs, ... }:

{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.packages = [
    # tools
    pkgs.git
    pkgs.htop
    pkgs.tmux

    # utils
    pkgs.jq
  ];

  programs.git = {
    enable = true;
    userName = "hiepph";
    userEmail = "hiepph@tuta.io";
    extraConfig = {
      core.editor = "vim";
    };
  };

  # home.file.".vimrc".source = ./.vimrc;
  # home.file.".tmux.conf".source = ./.tmux.conf;
}
