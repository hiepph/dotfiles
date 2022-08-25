# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

with pkgs;
{
  programs.fish = {
    enable = false;

    plugins = [
      {
        name = "fish-kubectl-completions";
        src = pkgs.fetchFromGitHub {
          owner = "evanlucas";
          repo = "fish-kubectl-completions";
          rev = "ced676392575d618d8b80b3895cdc3159be3f628";
          sha256 = "09qcj82qfs4y4nfwvy90y10xmx6vc9yp33nmyk1mpvx0dx6ri21r";
        };
      }
    ];

    functions = {
      #
      # Examples:
      #
      # $ trim "    a    long string"
      # => "a long string"
      #
      # $ echo "    another long   string" | trim
      # => "another long string"
      #
      trim = {
        description = "Clear all leading, trailing spaces at the beginning, the end and the middle of the string";
        body = ''
function cmd
    sed 's/ *//' | tr -s ' '
end

if test $argv[1]
    echo $argv[1] | cmd
else
    read -z | cmd
end
'';
      };

      #
      # Examples:
      #
      # $ gitignore python
      #
      gitignore = {
        description = "Get the content of .gitignore for a language";
        body = "curl -sL https://www.gitignore.io/api/$argv";
      };
    };
  };

  home.packages = [
    #
    # dev
    #

    git
    git-secret

    # editor
    ispell
    ctags

    # build
    cmake
    nasm

    # db
    postgresql_13
    mysql

    # shell
    direnv # https://direnv.net/
    stow
    indent
    jump # navigate faster
    tmux
    gnupg
    (callPackage ./packages/ripgrep {}) # grep alternative
    fd # find alternative
    bat
    watch
    (callPackage ./packages/reflex {}) # file watcher
    fzf
    imagemagick
    pv # cat, with progress
    ranger # file browser

    # helper
    hugo # static site generator
    httpie # RESTful client
    jq
    yq
    jo # easy json construction

    # virtual
    qemu

    # languages
    boot # clojure build tool
    zprint # clojure formatter
    babashka # interpreter for Clojure scripting
    (callPackage ./packages/julia {})
    R
    (callPackage ./packages/lua {})
    go
    (callPackage ./packages/gotools {})
    ruby
    (callPackage ./packages/zig {})
    graphviz
    # java build tool
    gradle
    maven

    #
    # devops
    #

    # backup
    borgbackup

    # infrastructure
    (callPackage ./packages/terraform {})
    awscli2
    kops
    kubectx
    k9s
    minikube
    google-cloud-sdk
    (callPackage ./packages/helm {})

    # secret
    (callPackage ./packages/bitwarden-cli {})
    pass

    # template
    vagrant

    # networking
    nmap
    sshpass
    rclone
    wget

    # monitoring
    htop
  ];
}
