# refer: https://daiderd.com/nix-darwin/manual/index.html
{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [ # nix
      home-manager

      # tools
      wget

      # text editor
      vim

      # languages
      python38
    ];

  # cron job with launchd
  launchd.user.agents = {

    # need to grant /bin/zsh "Full Disk Access" in "Privacy and Security"
    # since Borg needs to access `/Volumes`
    backup.serviceConfig = {
      Label = "local.hiepph.backup";
      RunAtLoad = true;
      StandardOutPath = "/tmp/launchd/local.hiepph.backup/log.out";
      StandardErrorPath = "/tmp/launchd/local.hiepph.backup/log.err";
      StartCalendarInterval = [
       { Hour = 12; }
      ];
      Program = "/Users/hiepph/scripts/_backup";
    };
  };


  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

  # Cronjob for garbage collector
  nix.gc = {
    user = "hiepph";
    automatic = true;
    interval = { Hour = 12; Minute = 0; };
    options = "--delete-older-than 30d";
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
