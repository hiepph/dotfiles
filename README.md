# dotfiles

![gif](https://thumbs.gfycat.com/AlarmingCoordinatedEarwig-max-1mb.gif)


## Bootstrap

[bootstrap.yml](./bootstrap.yml)

All bootstrap setup scripts and configurations are managed by Ansible.

Edit `/etc/ansible/hosts`:

```
[local]
localhost    ansible_connection=local
```

Then run the playbook:

```
ansible-playbook -l localhost bootstrap.yml -t <tag>
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


## Themes

Below is an awesome list of themes (arts) done in style. Mostly are eye-care themes.
Maybe I'm just in awe of their landing pages and scientific research.
But if the author does care about your eye health, his/her art is worth trying.

+ [Nord](https://www.nordtheme.com/). Scandinavian, northern, Aurora
  Borealis inspiration.

+ [Solarized](https://ethanschoonover.com/solarized/). Taoist, yin-yang vibe.

+ [Plan9/Acme](https://github.com/john2x/plan9-theme.el). I read
somewhere (d'oh, I'm trying to find the source) that the original ACME
editor theme (classic yellow-tint background) was designed so you can stare at it all day without tiring your eyes.


\* On Dark vs Light

For a time I was on the dark side, and hated anything related to light themes.
Time changes. Now I realized there is no clear winner, but choose the
right theme for your workspace. Generally:

+ Dark mode: visual enhancement, suitable for dark environment, night
life. My terminal and monitoring theme is always dark.

+ Light mode: suitable for reading and coding, light environment. I
  found it extremely hard to focus on the text with dark background
  and white text. It feels unnatural (from a perspective of a book lover).

+ Reduce the habit of switching too frequently between dark and light
  themes (e.g. dark mode and light documentation). Your eyes have to
  re-calibrate too much thus quickly cause eye strain.
