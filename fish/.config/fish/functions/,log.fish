#
# USAGE:
#
# - To write normally to stdout:
#
#   $ ,log "The program ran well."
#
# - To write to stderr:
#
#   $ ,log -e "Something happened!"
#   $ ,log --error "Something happend!"
#
function ,log --description "Log a message"
    argparse --ignore-unknown 'e/error'  -- $argv

    if set -q _flag_error
        set_color red
        echo -e "[ERROR] $argv[1..]" >&2
    else
        set_color green
        echo -e "[INFO] $argv[1..]"
    end

    set_color normal
end
