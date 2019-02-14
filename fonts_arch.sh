mkdir $HOME/src

# use trizen for AUR
sudo pacman -S trizen

# powerline
git clone https://github.com/powerline/fonts.git $HOME/src/powerline-fonts --depth 1
cd $HOME/src/powerline-fonts
./install.sh

# basic fonts for icons
trizen -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji fira-code-git ttf-font-awesome
