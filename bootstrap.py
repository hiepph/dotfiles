# Bootstrap the whole newly installed system
# supports: Arch
import argparse
import subprocess
import sys


def execute(cmd):
    _cmd = list(map(lambda l: l.strip().split(' '), filter(
        lambda c: len(c.strip()) > 0, cmd.split('\n'))), )
    list(
        map(subprocess.run, _cmd))


_pacman = "pacman -S --needed --noconfirm"


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


def bootstrap_locale():
    execute(f"""
    localedef -f UTF-8 -i en_US en_US.UTF-8
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
    pacman -S --needed nvidia
    """)


def bootstrap_vim():
    execute(f"""
    curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    touch $HOME/.custom.vim
    vim -c "PlugInstall" -c "qa"
    """)


def bootstrap_tmux():
    execute(f"""
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
    """)


def bootstrap_fcitx():
    execute(f"""
    {_pacman} fcitx fcitx-unikey fcitx-im
    """)


def bootstrap_font():
    execute(f"""
    {_pacman} powerline powerline-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts
    """)
    bootstrap_locale()


def bootstrap_ibus():
    #execute(f"""
    #{_pacman} ibus ibus-unikey
    #""")

    # replace default python with /usr/bin/python3 in script file
    f = '/usr/bin/ibus-setup'
    lines = open(f).readlines()

    tbr = 'exec /usr/bin/python3 /usr/share/ibus/setup/main.py $@'
    _lines = list(
        map(lambda l: tbr if '/usr/share/ibus/setup/main.py' in l else l, lines)
    )
    new_content = ''.join(_lines)

    open(f, 'w').write(new_content)


def bootstrap_go():
    execute(f"""
    go get golang.org/x/tools/cmd/goimports
    """)


def bootstrap_conda():
    execute(f"""
    cd ~/Downloads
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
    bash Miniconda3-latest-Linux-x86_64.sh -s
    """)


def bootstrap_ux():
    """Dunst: notification daemon
    Rofi: window switcher and dmenu replacement
    """
    bootstrap_locale()
    execute(f"""
    {pacman} dunst rofi alttab-git
    """)


M = {'git': bootstrap_git,
     'nvidia': bootstrap_nvidia,
     'aur': bootstrap_aur,
     'vim': bootstrap_vim,
     'tmux': bootstrap_tmux,
     'font': bootstrap_font,
     'ibus': bootstrap_ibus,
     'go': bootstrap_go,
     'locale': bootstrap_locale,
     'conda': bootstrap_conda,
     'ux': bootstrap_ux,
     }


def info_and_exec(f):
    print(f"> Bootstraping {f}")
    M[f]()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('module', nargs='+',
                        help='module to install (default: all)')
    args = parser.parse_args()

    to_be_installed = args.module
    list(map(info_and_exec, to_be_installed))
