{ config, pkgs, ... }: # pkgs = import <nixpkgs> {};

let
  rootDir = "/../../..";
  pkgsUnstable = import <nixpkgs-unstable> {};
in
{
  imports = [

  ] ++ (
    if builtins.pathExists "${builtins.getEnv "HOME"}/customs/packages.nix"
    then ["${builtins.getEnv "HOME"}/customs/packages.nix"]
    else []
  );

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # security
    pkgs.gnupg
    pkgs.pinentry_mac

    # editor
    pkgs.vim
    pkgs.hunspell
    pkgs.wordnet

    # shell
    pkgs.coreutils # md5sum, ...
    pkgsUnstable.ripgrep # rusty grep
    pkgsUnstable.bat
    pkgsUnstable.fd # rusty find
    pkgsUnstable.fzf
    pkgs.stow
    pkgs.jump
    pkgs.tmux
    pkgs.direnv
    pkgsUnstable.jq
    pkgs.yq # jq for YAML
    pkgs.jo # JSON output from a shell
    pkgs.wget
    pkgs.htop
    pkgs.tokei # count your code, quickly
    pkgs.zoxide # smarter cd
    pkgs.pandoc # document format converter
    pkgsUnstable.nushell
    pkgsUnstable.duckdb # OLAP
    pkgs.du-dust # like du, but more intuitive
    pkgs.jc # to JSON!
    pkgs.speedtest-cli
    pkgs.imagemagick
    pkgs.hugo # static site generator
    pkgs.nodejs_20
    pkgs.atuin
    pkgs.tree

    # ops
    pkgsUnstable.awscli2
    pkgs.aws-vault
    pkgs.ssm-session-manager-plugin
    pkgsUnstable.eksctl
    pkgs.steampipe # query cloud resources with SQL
    pkgs.terraform
    pkgs.pulumi-bin
    pkgsUnstable.opentofu # Open Terraform fork
    pkgsUnstable.terragrunt # DRY terraform
    pkgs.rclone
    # pkgsUnstable.kubectx
    pkgsUnstable.tilt # docker-compose for Kubernetes
    pkgs.kind
    pkgs.minikube
    pkgs.nmap
    pkgs.inetutils
    # pkgs.vagrant
    # pkgs.packer
    pkgs.qemu
    pkgs.ansible
    pkgs.argocd # gitops
    pkgs.fluxcd # gitops
    pkgs.linkerd # service mesh
    pkgs.step-cli # https://github.com/smallstep/cli

    # languages
    pkgs.go
    pkgs.graphviz
    pkgs.asciidoctor
    # pkgs.sbcl

    # python
    pkgs.black # formatter
    pkgs.ruff # linter
    pkgs.yamllint

    # ruby
    # pkgs.solargraph # lsp
    # pkgs.rubocop # linter, formatter

    # go
    pkgs.gotools # goimports, gopls
    pkgs.delve # or `dlv` - debugger

    # Clojure
    pkgs.clojure
    pkgs.leiningen # project management tool
    pkgs.clj-kondo # linter
    pkgs.clojure-lsp

    # Database
    pkgs.postgresql

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'sqlite/.sqliterc' in
    # # the Nix store. Activating the configuration will then make '~/.sqliterc' a
    # # symlink to the Nix store copy.
    ".tmux.conf".source = ./. + "${rootDir}/tmux/.tmux.conf";
    ".sqliterc".source = ./. + "${rootDir}/sqlite/.sqliterc";
    ".ideavimrc".source = ./. + "${rootDir}/intellij/.ideavimrc";

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/hiepph/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.

  # Only available when set `programs.fish.enable = true`
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
