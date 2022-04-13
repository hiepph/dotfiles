# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

with pkgs;
{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  # refer: https://nixos.wiki/wiki/Zsh
  programs.zsh = {
    enable = true;

    prezto = {
      enable = true;
      prompt.theme = "pure";
    };

    sessionVariables = {
      "EDITOR" = "vim";
      "VISUAL" = "vim";
      "PATH" = "$HOME/scripts:$HOME/backup:$HOME/go/bin:/Library/Tex/texbin:$(gem environment gemdir)/bin:$HOME/.local/bin:$PATH";
    };

    shellAliases = {
      "k" = "kubectl";
    };

    initExtra = ''
# default edit everything with vim
export VISUAL=vim
export EDITOR=vim

# fzf binding keys
if [ -n "$\{commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

# enable direnv
eval "$(direnv hook zsh)"

# conda integration
eval "$(/Users/hiepph/miniconda3/bin/conda shell.zsh hook)"
# do not show (base)
export PROMPT=$(echo $PROMPT | sed 's/(base) //')
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
