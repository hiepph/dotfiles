## sudo

+ As root:
```
pkg install sudo
visudo
```

+ Config:
```
%wheel ALL=(ALL) ALL
```

## Essential packages

+ Shell:
```
sudo pkg install vim
```

+ Helpers:
```
sudo pkg stow xsel
```

## Fonts

- Basic fonts for icons:

```
sudo pkg install x11-fonts/firacode x11-fonts/powerline-fonts
```
