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

#
# languages
#

sudo port install rbenv ruby-build

# build dependencies for pyenv
sudo port install sqlite3 xz readline zlib tk lzma
