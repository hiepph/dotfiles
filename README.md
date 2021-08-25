# dotfiles

![gif](https://thumbs.gfycat.com/AlarmingCoordinatedEarwig-max-1mb.gif)


## Stow

For some applications (e.g. Emacs, Tmux, Vim), their configurations need to be symlinked to HOME.
This task is done by [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html).

```
stow <app>
```


## Bootstrap with Nix

My system is built with the help of [Nix](https://nixos.org/) for a functional and
reproducible configuration.

### Linux

+ Installation: refer [Linux](https://nix.dev/) guide


### Mac

+ Installation: refer [Mac](https://wickedchicken.github.io/post/macos-nix-setup/) guide.

+ Symlink `mac` to get specific configuration for `darwin` (`.nixpkgs/darwin-configuration.nix`):

```
stow mac
```

+ Rebuid configuration and switch:

```
darwin-rebuild switch
```


## Bootstrap with Ansible

`bootstrap/*.yml`

After the system is configured with Nix, all additional setup scripts
are managed by Ansible.

Edit `/etc/ansible/hosts`:

```
[local]
localhost    ansible_connection=local
```

Then run the playbook:

```
ansible-playbook -l localhost bootstrap/<sth>.yml -t <tag>
```


## Emacs

+ Config structure and performance optimization are referred from [doom-emacs](https://github.com/hlissner/doom-emacs/).
A nice explanation of how doom-emacs can achieve state-of-the-art startup time is on the reddit discussion: [How is Doom Emacs so damn fast](https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast)

+ Key bindings and literate functionalites are referred from [emacs-cmpitg](https://github.com/cmpitg/emacs-cmpitg)


## Vim

+ Normally `stow vim`

+ Create custom file for each machine: `~/.custom.vim`. For example:

```
colorscheme alduin
```


## Scripts

[scripts](./scripts)

Collection of my shortcut and useful scripts.

+ `_pocket`: query articles in [Pocket](https://app.getpocket.com/)
  - usage: `_pocket <query>` or `_pocket -q <query> -t <tag>`

+ `_docker_stop_all`: stop all running containers

+ `_gitignore`: download `.gitignore` file
  - usage: `_gitignore python`

+ `_g`: a simpler wrapper for [gh gist](https://cli.github.com/manual/gh_gist):
  - usage: `_g <query>` or `_g <query> -L 1000`


## Themes

Below is an awesome list of themes done in style. All are eye-care themes.

+ [Nord](https://www.nordtheme.com/)
+ [Solarized](https://ethanschoonover.com/solarized/)
+ [Plan9/Acme](https://github.com/john2x/plan9-theme.el)
