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


def bootstrap_git():
    # Git
    execute("""
    git config --global user.email "hiepph.2406@gmail.com"
    git config --global user.name "Hiep Pham"
    git config --global core.editor "vim"
    """)

# Nvidia
def bootstrap_nvidia():
    execute("""
    sudo pacman -S nvidia
    """)

M = {'git': bootstrap_git,
     'nvidia': bootstrap_nvidia,
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
