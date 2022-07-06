if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Customs for each machine
. ~/customs/tools.fish

# Abbreviations
abbr -a -g g 'git'
abbr -a -g k 'kubectl'

# Conda integration
eval ~/miniconda3/bin/conda "shell.fish" "hook" $argv | source
