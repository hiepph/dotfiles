set -x

#
# shell
#
sudo port install fish
sudo port install stow

#
# text editors
#

# CLI
sudo port install emacs +nativecomp +treesitter
# App. Refer: https://ports.macports.org/port/emacs-app/details/
sudo port install emacs-app +nativecomp +treesitter

#
# languages
#

# ruby
sudo port install rbenv ruby-build

# build dependencies for pyenv
sudo port install sqlite3 xz readline zlib tk lzma

# Kubernetes
sudo port install kubectl kubectx
