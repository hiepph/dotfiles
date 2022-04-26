# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

with pkgs;
{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  # refer: https://github.com/nix-community/home-manager/blob/master/modules/programs/fish.nix
  programs.fish = {
    enable = true;

    shellAbbrs = {
      "k" = "kubectl";
    };

    shellInit = ''
# Add some custom binary dirs
set -x PATH $HOME/scripts $HOME/backup $HOME/go/bin /Library/Tex/texbin (gem environment gemdir)/bin $HOME/.local/bin $PATH

# default edit everything with vim
set -x VISUAL vim
set -x EDITOR vim

# enable direnv
direnv hook fish | source

# conda integration
eval /Users/hiepph/miniconda3/bin/conda "shell.fish" "hook" $argv | source

# Integrate with `jump`
jump shell fish | source
'';
  };


  programs.git = {
    enable = true;
    userName = "hiepph";
    userEmail = "hiepph@tuta.io";
    extraConfig = {
      core.editor = "vim";
    };
  };

  home.packages = [
    # nix essential
    direnv # https://direnv.net/

    # backup
    borgbackup

    # build
    cmake
    hugo # static site generator
    nasm

    # db
    postgresql_13

    # dev
    git
    emacs
    ispell
    indent
    ctags
    httpie
    ansible
    watch
    jo # easy json construction
    jump # navigate faster

    # infrastructure
    terraform
    awscli2
    kops
    kubectx
    minikube
    google-cloud-sdk

    # networking
    nmap
    sshpass
    rclone
    wget

    # languages
    boot # clojure build tool
    zprint # clojure formatter
    babashka # interpreter for Clojure scripting
    (callPackage ./packages/julia {})
    (callPackage ./packages/lua {})
    go
    (callPackage ./packages/gotools {})
    ruby
    (callPackage ./packages/zig {})
    graphviz

    # shell
    htop
    tmux
    gnupg
    pass
    (callPackage ./packages/ripgrep {})
    bat
    (callPackage ./packages/reflex {})
    jq
    stow
    fzf

    # virtualization
    qemu
    vagrant
  ];
}
