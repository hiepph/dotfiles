if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Customs for each machine
if test -f ~/customs/fish.fish
    . ~/customs/fish.fish
end

# Abbreviations
abbr -a -g g 'git'
abbr -a -g k 'kubectl'

# enable direnv
if type -q direnv
    direnv hook fish | source
end

# prevent weird behaviour with tmux
# ref: http://nicksun.fun/linux/2020/10/07/tmux-macos.html
# if test -n "$TMUX"
#  conda deactivate && conda activate base
# end

# integrate jump
# refer: https://github.com/gsamokovarov/jump
if type -q jump
    jump shell fish | source
end

# default edit everything with vim
set -x VISUAL vim
set -x EDITOR vim

# Conda integration
status is-interactive && eval ~/miniconda3/bin/conda "shell.fish" "hook" $argv | source
