## SSH

Generate SSH key:

```
$ ssh-keygen -t rsa -b 4096 -C "hoanghiepjp96@gmail.com"
```

## Terminal

* Prezto: https://github.com/sorin-ionescu/prezto

```
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -fs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

chsh -s /bin/zsh
```

* FZF: https://github.com/junegunn/fzf.git

```
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

## Vim

+ Plugins managed with [vim-plug](https://github.com/junegunn/vim-plug#installation), install with:

```
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

Then in `vim`: `[NORMAL] :PlugInstall`

## Tmux

+ Go into a tmux session and install plugins:

```
Prefix + I
```

## Fonts

* Source:

+ https://github.com/ryanoasis/nerd-fonts.git

+ https://github.com/powerline/fonts.git

* Basic fonts for icons:

```
pacaur -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome
```
