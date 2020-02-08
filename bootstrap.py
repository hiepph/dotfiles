# Bootstrap the whole newly installed Arch system
import argparse
import subprocess
import sys


def execute(cmd):
    _cmd = list(
        map(lambda l: l.strip().split(' '), filter(lambda c: len(c.strip()) > 0, cmd.split('\n'))),
    )
    list(
        map(subprocess.run, _cmd))


_pacman = "sudo pacman -S --needed --noconfirm"


def bootstrap_aur():
    """Use yay for AUR package manager
    ref: https://github.com/Jguer/yay
    """
    execute(f"""
    {_pacman} binutils make gcc
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
    """)


def bootstrap_git():
    execute(f"""
    {_pacman} git
    git config --global user.email "hiepph.2406@gmail.com"
    git config --global user.name "Hiep Pham"
    git config --global core.editor "vim"
    """)


def bootstrap_nvidia():
    execute("""
    sudo pacman -S --needed nvidia
    """)


def bootstrap_vim():
    execute(f"""
    {_pacman} vim
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    touch ~/.custom.vim
    vim -c "PlugInstall" -c "qa"
    """)


def bootstrap_tmux():
    execute(f"""
    {_pacman} tmux
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    """)


M = {'git': bootstrap_git,
     'nvidia': bootstrap_nvidia,
     'aur': bootstrap_aur,
     'vim': bootstrap_vim,
     'tmux': bootstrap_tmux,
    }


def info_and_exec(f):
    print(f"> Bootstraping {f}")
    M[f]()


if __name__ == '__main__':
    if len(sys.argv) == 1:
        # no additional arguments
        to_be_installed = M.keys()
    else:
        parser = argparse.ArgumentParser()
        parser.add_argument('module', nargs='+',
                            help='module to install (default: all)')
        args = parser.parse_args()

        to_be_installed = args.module

    print('> Modules to be installed:', *to_be_installed)
    print('> Continue? [y/n]', end=' ')
    if input() == 'y':
        list(
            map(info_and_exec, to_be_installed)
        )
    else:
        sys.exit(0)
