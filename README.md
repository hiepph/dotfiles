# Overview

![demo](https://i.imgur.com/TakDKKE.png)

`dotfiles` is managed with [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html).
Simply integrate config with `stow package`.


## Basic OS setup

+ Arch: `Arch/Arch.md`

+ FreeBSD: `FreeBSD/FreeBSD.md`


## SSH

Generate SSH key:

```sh
ssh-keygen -t rsa -b 4096 -C "hoanghiepjp96@gmail.com"
```


## Git

```sh
git config --global user.email "hoanghiepjp96@gmail.com"
git config --global user.name "Hiep Pham"
```


## Shell

* [Prezto](https://github.com/sorin-ionescu/prezto)

    + Setup:

    ```sh
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

    setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
      sudo ln -fs "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done

    chsh -s /bin/zsh
    ```

    + Update:

    ```sh
    git pull origin master
    git submodule update --init --recursive
    ```


* [FZF](https://github.com/junegunn/fzf.git)

```sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```


## Tmux

+ Tmux config for user and root

```sh
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
sudo ln -s /m/config/tmux/tmux.conf /root/.tmux.conf
sudo git clone https://github.com/tmux-plugins/tpm /root/.tmux/plugins/tpm
```

+ Go into a tmux session and install plugins:

```sh
Prefix + I
```

+ To update plugins:

```sh
Prefix + U
```


## Vim

+ Plugins managed with [vim-plug](https://github.com/junegunn/vim-plug#installation), install with:

```
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

+ Then in vim:

```sh
:PlugInstall
```

+ To update all plugins:

```sh
:PlugUpdate
```


## Awesome

+ Support widgets:

```sh
git clone https://github.com/deficient/battery-widget.git ~/.config/awesome/battery-widget
```
