#
# USAGE:
# - Run a command. The command and its argument are logged to stdout.
#   Result's stderr is *omitted* unless there is an error.
#   When there is an error, the script exits with result's status code.
#
#   $ ,sh ls -alh
#
function ,sh --description "Execute a command, verbosely"
    ,log "\$ $argv"

    $argv
    set code $status

    if test $status -ne 0
        ,log --error "Command failed: \"$argv\" with status code: $code"
        exit $code
    end
end
