#
# USAGE:
#
# - To write normally to stdout:
#
#   $ ,log "The program ran well."
#
#   [OUT]> The program ran well.
#
# - To write to stderr:
#
#   $ ,log -e "Something happened!"
#   $ ,log --error "Something happend!"
#
#   [ERR]> Error
#
function ,log --description "Log a message to the stderr"
    argparse --ignore-unknown 'e/error'  -- $argv

    if set -q _flag_error
        echo -e "\tERR> $argv[1..]" >&2
    else
        echo -e "\tOUT> $argv[1..]"
    end
end
