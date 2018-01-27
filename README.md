# Overview

![demo](https://i.imgur.com/r7Gcmxe.png)

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

    * Install:

    ```sh
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
    ```

    * Ripgrep fusion:

    ```sh
    # ~/.zshrc
    export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
    ```

    * Update:

    ```sh
    cd ~/.fzf && git pull && ./install
    ```


### Termite

```sh
mkdir -p ~/.config/termite
ln -s ~/dotfiles/termite/gruvbox ~/.config/termite/config

# ~/.zshrc
export TERM=xterm-256color
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


## IBus

```sh
# .xinitrc
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx
```
