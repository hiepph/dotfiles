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

+ Init key:

```sh
sudo pacman-key --init
sudo pacman-key --populate archlinux
```

+ Add mirros:

```sh
pacaur -S reflector
reflector --latest 10 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist --verbose
pacaur -Syy
```


## Essential packages

```
pacaur -S vim termite tmux fasd stow xsel ibus ibus-qt ibus-unikey ibus-anthy pulseaudio alsamixer xbindkeys
```


## Utilities

```
pacaur -S redshift zathura zathura-pdf-poppler
```


## Fonts

- Source:

    + https://github.com/ryanoasis/nerd-fonts.git

    + https://github.com/powerline/fonts.git

- Basic fonts for icons:

```
pacaur -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome fira-code-git
```
