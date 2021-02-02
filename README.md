# dotfiles

![gif](https://thumbs.gfycat.com/AlarmingCoordinatedEarwig-max-1mb.gif)


## Bootstrap

[bootstrap.yml](./bootstrap.yml)

All bootstrap setup scripts and configurations are managed by Ansible.

```
ansible-playbook bootstrap.yml -t <app>
```

For some setups root priviledge is required:

```
ansible-playbook bootstrap.yml -K -t <app>
```

For some applications (e.g. Emacs, Tmux, Vim), their configurations need to be symlinked to HOME.
This task is done by [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html).

```
stow <app>
```


## Emacs

+ Config structure and performance optimization are referred from [doom-emacs](https://github.com/hlissner/doom-emacs/).
A nice explanation of how doom-emacs can achieve state-of-the-art startup time is on the reddit discussion: [How is Doom Emacs so damn fast](https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast)

+ Key bindings and literate functionalites are referred from [emacs-cmpitg](https://github.com/cmpitg/emacs-cmpitg)

+ Daemon management (refer [scripts](#scripts)):

  + Start daemon: `_emacs start`
  + Stop daemon: `_emacs stop`
  + Create a new frame for editing: `_emacs`
  + Restart: `_emacs restart`

## Scripts

[scripts](./scripts)

Collection of my shortcut and useful scripts.

+ `_pocket`: query articles in [Pocket](https://app.getpocket.com/)
  - usage: `_pocket <query>` or `_pocket -q <query> -t <tag>`

+ `_docker_remove_dangling`: remove all dangling (`<none>`) containers

+ `_docker_stop_all`: stop all running containers

+ `_gitignore`: download `.gitignore` file
  - usage: `_gitignore python`

+ `_g`: a simpler wrapper for [gh gist](https://cli.github.com/manual/gh_gist):
  - usage: `_g <query>` or `_g <query> -L 1000`
