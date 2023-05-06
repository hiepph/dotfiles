# Stow

Mainly I structure my configurations follow their paths starting from
$HOME. To activate the configuration, symlink the corresponding folder
to $HOME. This task is done by [GNU Stow](https://www.gnu.org/software/stow/manual/stow.html).

```
stow <conf>
```

# Linux

Some distros I'm using for my personal machine:

+ [OpenSUSE Tumbleweed](https://www.opensuse.org/)
  + Rolling released.
  + System recovery and snapshot with [Snapper](https://doc.opensuse.org/documentation/leap/reference/html/book-reference/cha-snapper.html) and [btrfs](https://en.wikipedia.org/wiki/Btrfs)
  + [YaST](https://yast.opensuse.org/) is awesome.

+ [Fedora](https://getfedora.org/)
  + Supports a wide range of hardware.
  + Bleeding edge packages.
  + btrfs is the default filesystem since Fedora 36.

+ [Arch](https://archlinux.org/):
  + Rolling released.
  + Total control of my system. KISS (Keep It Simple, Stupid).
  + Arch's [wiki](https://wiki.archlinux.org/) is awesome.
  + Have fun ricing my desktop.

# BSD

*TBD*


# Mac

[Homebrew](https://brew.sh/) and [Nix](https://nixos.org/) are used to manage packages.

But why mixing instead of committing to one?

- I like Nix, but it doesn't support some packages for Darwin, yet.
- Homebrew, on the other hand, has strong support from the community and I can find almost any package (e.g. `emacs`). But it messes with my systems (a.k.a `/usr/bin`) so I only use it to install some necessary packages. Sometimes I do like Homebrew to mutate my systems (e.g. `fish` shell).


## Homebrew

```bash
cd homebrew
brew bundle
```

## Nix

- Use the `nix-installer` from [Determinate Systems](https://zero-to-nix.com/) to install Nix on my Mac.

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```


- Install [home-manager](https://nix-community.github.io/home-manager/index.html):

```bash
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

set NIX_PATH $HOME/.nix-defexpr/channels /nix/var/nix/profiles/per-user/root/channels
nix-shell '<home-manager>' -A install
```

- Shell are not managed with `home-manager` though, for stability reason.


- When you need to make change to the packages, edit `~/.config/home-manager/home.nix` and:

```bash
home-manager switch
```

## Hammerspoon: desktop automation

I missed [i3](https://i3wm.org/) on my Mac. I tried [Rectangle](https://rectangleapp.com/) to bring
back the feeling of tiling windows but it lacks the power of declarative configuration.

Hammerspoon solves this problem by being a bridge to Mac's API. It uses Lua as scripting language.


# Shell

+ My default shell is [fish](https://fishshell.com/).

  + Pros:
    - I hate Bash arcane syntax.  It's fast to write a small script in Bash but horrible to grow into a larger script. Even [Google Shell style guide](https://google.github.io/styleguide/shellguide.html#when-to-use-shell) recommends not to write a script more than 100 lines long.
    - Seamless interops with shell commands. It makes writing script interactively on a shell fun. One-liner manner is achievable. With a general purpose language, e.g. Python, I have to wrap shell command with `os.system` or `subprocess`.
    - References: [Why fish?](https://fishshell.com/docs/current/tutorial.html#why-fish), [Rash lang](https://youtu.be/Acjqx1MPkw4).

  + Cons:
    - It does not follow POSIX shell standards. So if I want to run a common Bash or Zsh script, better use `bash script.sh`.


# Emacs

+ Config structure and performance optimization are referred from [doom-emacs](https://github.com/hlissner/doom-emacs/).
A nice explanation of how doom-emacs can achieve state-of-the-art startup time is on the reddit discussion: [How is Doom Emacs so damn fast](https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast)

+ Key bindings and literate functionalites are referred from [emacs-cmpitg](https://github.com/cmpitg/emacs-cmpitg)


# Vim

+ Create custom file for each machine: `~/.custom.vim`. For example:

```
colorscheme alduin
```


# Themes

Below is an awesome list of themes done in style. All are eye-care themes.

+ [Nord](https://www.nordtheme.com/)
+ [Solarized](https://ethanschoonover.com/solarized/)
+ [Plan9/Acme](https://github.com/john2x/plan9-theme.el)
