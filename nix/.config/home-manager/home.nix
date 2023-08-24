{ config, pkgs, ... }:

let
  rootDir = "/../../..";
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "hiepph";
  home.homeDirectory = "/Users/hiepph";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "22.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # ops
    pkgs.google-cloud-sdk
    pkgs.awscli2
    pkgs.steampipe # query cloud resources with SQL
    pkgs.terraform
    pkgs.rclone
    pkgs.pass
    pkgs.kubectx
    pkgs.minikube
    pkgs.nmap
    pkgs.inetutils
    pkgs.vagrant
    pkgs.qemu
    pkgs.colima # Container runtimes on macOS (and Linux) with minimal setup

    # dev
    pkgs.cmake
    pkgs.nasm

    # languages
    pkgs.go
    pkgs.graphviz

    # editor
    pkgs.emacs
    pkgs.hunspell

    # shell
    pkgs.ripgrep
    pkgs.bat
    pkgs.fd
    pkgs.fzf
    pkgs.stow
    pkgs.jump
    pkgs.tmux
    pkgs.direnv
    pkgs.jq
    pkgs.yq
    pkgs.jo
    pkgs.wget
    pkgs.htop
    pkgs.tokei # count your code, quickly
    pkgs.zoxide # smarter cd

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
    ".hammerspoon".source = modules/hammerspoon/.hammerspoon;
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
