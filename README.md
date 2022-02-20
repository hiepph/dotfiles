# Stow

Mainly I structure my configurations follow their paths starting from
$HOME. To activate the configuration, symlink the corresponding folder
to $HOME. This task is done by [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html)

```
stow <conf>
```

# Mac

## Nix: functional package manager

My system is built with the help of [Nix](https://nixos.org/) for a functional and
reproducible configuration.

+ Install `Nix`: https://nix.dev/tutorials/install-nix#macos

+ Install `nix-darwin`: refer [here](https://daiderd.com/nix-darwin/)

```
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer
```

+ Symlink for system configuration:

```
nix-shell -p stow --command "stow mac"
```

+ Build and switch the whole system:

```
darwin-rebuild switch
```

## Nix: home manager

`nixos-rebuild` or `darwin-rebuild` is used for the whole system.
Home manager is specific for each user.

+ Edit `.config/nixpkgs/home.nix` for each local machine, for example:

```
{
  ...
  imports = [
    ./main/general.nix
    ./main/roles/mac/index.nix
  ];

  ...
}
```

where, `general.nix`: cross-platform configurations.


+ Make change and switch environment:

```
home-manager switch
```

## Hammerppoon: desktop automation

`mac/.hammerspoon`

I missed [i3](https://i3wm.org/) on my Mac. I tried [Rectangle](https://rectangleapp.com/) to bring
back the feeling of tiling windows but it lacks the power of declarative configuration.

Hammerspoon solves this problem by being a bridge to Mac's API. It uses Lua as scripting language.


## References

+ [Hugo Reeves](https://hugoreeves.com/posts/2019/nix-home/)

# Linux

*TBD*

# Emacs

+ Config structure and performance optimization are referred from [doom-emacs](https://github.com/hlissner/doom-emacs/).
A nice explanation of how doom-emacs can achieve state-of-the-art startup time is on the reddit discussion: [How is Doom Emacs so damn fast](https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast)

+ Key bindings and literate functionalites are referred from [emacs-cmpitg](https://github.com/cmpitg/emacs-cmpitg)

+ Create custom file for each machine
  `~/.emacs.d/core/core-custom.el`, e.g.:

```
;; theme
(use-package plan9-theme)
(load-theme 'plan9 t)

;; font
(set-frame-font "Input Mono" nil t)
(add-to-list 'default-frame-alist
             '(font . "Input Mono"))

(provide 'core-custom)
```

# Vim

+ Create custom file for each machine: `~/.custom.vim`. For example:

```
colorscheme alduin
```


# Scripts

[scripts](./scripts)

Collection of my shortcut and useful scripts.

# Themes

Below is an awesome list of themes done in style. All are eye-care themes.

+ [Nord](https://www.nordtheme.com/)
+ [Solarized](https://ethanschoonover.com/solarized/)
+ [Plan9/Acme](https://github.com/john2x/plan9-theme.el)
