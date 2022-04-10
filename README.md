# Stow

Mainly I structure my configurations follow their paths starting from
$HOME. To activate the configuration, symlink the corresponding folder
to $HOME. This task is done by [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html)

```
stow <conf>
```

Some modules don't follow this strategy (e.g. *scripts*, *backup*) and I have to it manually. Currently I'm researching for a unified solution.

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

+ Edit `.config/nixpkgs/home.nix`:

```
{
  ...
  imports = [
    ./main.nix
  ];

  ...
}
```

+ Make change and switch environment:

```
home-manager switch
```

## Hammerspoon: desktop automation

`mac/.hammerspoon`

I missed [i3](https://i3wm.org/) on my Mac. I tried [Rectangle](https://rectangleapp.com/) to bring
back the feeling of tiling windows but it lacks the power of declarative configuration.

Hammerspoon solves this problem by being a bridge to Mac's API. It uses Lua as scripting language.

## References

+ [Hugo Reeves](https://hugoreeves.com/posts/2019/nix-home/)

# Linux

*TBD*

# Ansible

I use Ansible to setup some post-configurations or run some common tasks.

## Setup

1. Edit `/etc/ansible/hosts`:

```
[local]
localhost   ansible_connection=local
```

2. Now I can run Ansible for my local machine. For example:

```
ansible-playbook -l local ansible/ruby/main.yml
```

## Why Ansible when I already have Nix?

Nix is indeed great for installing binary applications. But it is too complicated in setting up a language's ecosystem, e.g. Ruby gems, Python packages, Go packages. Don't even let me mention rvm or miniconda integration.

I often find myself banging my head to figure out how to properly make things work (e.g. [Rails on Nix](https://actually.fyi/posts/rails-on-nix/)). Nix is trying to be too intelligent to isolate the package but on the other hand make the process ten times painful.

We can all agree making an immutable system is hard. But I don't want to trade that fantasy with an overly complicated design to make that happen. Nix has potential but falls short with its current state.

On the other hand, Ansible requires me to run an additional command and make changes to the system. Simple and intuitive. Everything works as it should be.

So my choice would be:

+ Use Nix for binary applications, packages or simple linking problems.
+ Ansible for a custom ecosystem.

## Python

Python versions, packages and environments are managed with **miniconda**.

Install with `ansible`. It will install `miniconda` and all of essential packages with default environment `base`.

```bash
cd ansible/python

ansible-playbook -l local main.yml
```

*Notes:* After you successfully install `miniconda`, if you want to install those essential packages with a custom conda environment:

```bash
cd ansible/python

ansible-playbook -l local main.yml -e "conda_env=myenv"
```

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

To use: `ln -s $PWD/scripts scripts`


# Backup

3-2-1 strategy.

To use: `ln -s $PWD/backup/scripts backup`

# Secret manager

I use [Pass](https://www.passwordstore.org/) to manage my secrets and passwords.

+ To add my storage repository: `pass git remote add origin <super-secret-repository>`

+ To pull my encrypted content from my secret repostiory: `pass git pull`

+ To show a secret: `pass show aws/personal/ACCESS_KEY`

+ To update content to the repository: `pass git push`

# Themes

Below is an awesome list of themes done in style. All are eye-care themes.

+ [Nord](https://www.nordtheme.com/)
+ [Solarized](https://ethanschoonover.com/solarized/)
+ [Plan9/Acme](https://github.com/john2x/plan9-theme.el)
