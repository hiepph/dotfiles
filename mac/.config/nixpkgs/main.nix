# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    # test
    pytest

    # linter and formatter
    yamllint
    (callPackage ./packages/black {})

    # visualization
    matplotlib

    # tools
    ansible
  ];
  python-with-my-packages = pkgs.python38.withPackages my-python-packages;
in {
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
      "PATH" = "$HOME/scripts:$HOME/go/bin:/Library/Tex/texbin:$(gem environment gemdir)/bin:$HOME/.local/bin:$PATH";
    };

    shellAliases = {
      "k" = "kubectl";
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

    # db
    postgresql_13

    # dev
    git
    emacs
    ispell
    indent
    ctags
    httpie

    # infrastructure
    terraform
    awscli2
    kops
    kubectx
    minikube

    # networking
    nmap
    sshpass
    rclone

    # languages
    python-with-my-packages
    boot # clojure build tool
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
    vagrant
  ];
}
