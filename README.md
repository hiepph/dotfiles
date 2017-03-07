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

* Plugins:

+ https://github.com/jiangmiao/auto-pairs

## Fonts

* Source: 

+ https://github.com/ryanoasis/nerd-fonts.git

+ https://github.com/powerline/fonts.git

* Basic fonts for icons:

```
pacaur -S ttf-dejavu ttf-liberation terminus-font noto-fonts noto-fonts-cjk noto-fonts-emoji
```
