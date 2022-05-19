# ref: https://rycee.gitlab.io/home-manager/options.html
{ config, lib, pkgs, ... }:

with pkgs;
{
  home.sessionVariables = {
    EDITOR = "vim";
  };

  # ref: https://github.com/nix-community/home-manager/blob/master/modules/programs/fish.nix
  programs.fish = {
    enable = true;

    shellAbbrs = {
      "k" = "kubectl";
      "g" = "git";
      "l" = "ls";
      "d" = "docker";
    };

    shellInit = ''
# Add some custom binary dirs
set -x PATH $HOME/scripts $HOME/backup $HOME/go/bin /Library/Tex/texbin (gem environment gemdir)/bin $HOME/.local/bin $HOME/.nix-profile/bin/ $PATH

# default edit everything with vim
set -x VISUAL vim
set -x EDITOR vim

# enable direnv
direnv hook fish | source

# conda integration
eval /Users/hiepph/miniconda3/bin/conda "shell.fish" "hook" $argv | source

# prevent weird behaviour with tmux
# ref: http://nicksun.fun/linux/2020/10/07/tmux-macos.html
if test -n "$TMUX"
   conda deactivate && conda activate base
end

# Integrate with `jump`
jump shell fish | source
'';

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

  programs.git = {
    enable = true;
    userName = "hiepph";
    userEmail = "hiepph@tuta.io";
    extraConfig = {
      core.editor = "vim";
    };
  };

  home.packages = [
    #
    # dev
    #

    # editor
    git
    emacs
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
    (callPackage ./packages/ripgrep {})
    bat
    watch
    (callPackage ./packages/reflex {}) # file watcher
    fzf
    imagemagick

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
    jdk11 # Java

    #
    # devops
    #

    # backup
    borgbackup

    # infrastructure
    terraform
    awscli2
    kops
    kubectx
    minikube
    google-cloud-sdk
    (callPackage ./packages/helm {})

    # secret
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
