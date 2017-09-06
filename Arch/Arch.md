## Package manager

+ Use `pacaur` over `pacman`:

```sh
./pacaur_install.sh
```

+ Config `pacman`:

```sh
# /etc/pacman.conf
SigLevel = Optional TrustAll
# SigLevel = Never # This skips validity check, kind of dangerous
```

And now:

```sh
sudo pacman-key --init
sudo pacman-key --populate archlinux
sudo pacman -Syy
```


## Essential packages

```
pacaur -S vim termite tmux fasd stow xsel ibus ibus-qt ibus-unikey ibus-anthy
```


## Utilities

```
pacaur -S redshift
```


## Fonts

- Source:

    + https://github.com/ryanoasis/nerd-fonts.git

    + https://github.com/powerline/fonts.git

- Basic fonts for icons:

```
pacaur -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome fira-code-git
```
