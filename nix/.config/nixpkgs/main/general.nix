# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

let
  my-python-packages = python-packages: with python-packages; [
    pylint
    autopep8

    ansible
    speedtest-cli
  ];
  python-with-my-packages = pkgs.python38.withPackages my-python-packages;
in {
  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.packages = with pkgs; [
    # nix essential
    direnv # https://direnv.net/

    # languages
    python-with-my-packages

    # tools
    git
    htop
    tmux
    gnupg
    pass

    # utils
    jq
    stow
    fzf

    # devops
    sshpass
    rclone
  ];

  programs.zsh = {
    enable = true;

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

  programs.autojump = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "hiepph";
    userEmail = "hiepph@tuta.io";
    extraConfig = {
      core.editor = "vim";
    };
  };
}
