## Package manager

+ Use `trizen` over `pacman`:

```sh
sudo pacman -S trizen
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
trizen -S reflector
reflector --latest 10 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist --verbose
trizen -Syy
```


## Essential packages

```
trizen -S vim termite tmux fasd stow xsel ibus ibus-qt ibus-unikey ibus-anthy pulseaudio alsamixer xbindkeys
```


## Utilities

```
trizen -S redshift zathura zathura-pdf-poppler
```


## Fonts

- Basic fonts for icons:

```
trizen -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome fira-code-git powerline-fonts-git
```
