set -x

#
# shell
#
sudo port install fish
sudo port install git

#
# text editors
#

# refer: https://ports.macports.org/port/emacs-app/details/
sudo port install emacs-app +nativecomp +treesitter

#
# ops
#

sudo port install borgbackup borgmatic
sudo port install pulumi

sudo port install helm
sudo port select --set helm helm3.13

#
# languages
#
sudo port install rbenv ruby-build
sudo port install julia

# build dependencies for pyenv
sudo port install sqlite3 xz readline zlib tk lzma
