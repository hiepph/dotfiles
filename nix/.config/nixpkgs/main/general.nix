# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.packages = with pkgs; [
    # nix essential
    direnv # https://direnv.net/

    # tools
    git
    htop
    tmux

    # utils
    jq
    ripgrep
    stow
    fzf

    # devops
    ansible
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

  # home.file.".vimrc".source = ./.vimrc;
  # home.file.".tmux.conf".source = ./.tmux.conf;
}
