function log_error
    echo -e "\t> $argv[1]" >&2
end


function ,sh --description "Execute a command, verbosely"
    echo -e "\t\$ $argv"

    set tmp_dir (mktemp -d)
    set err_log $tmp_dir/,sh.err

    $argv[1..-1] 2>$err_log
    set code $status

    if test $status -ne 0
        log_error "Command failed: \"$argv[1..-1]\" with status code: $code"

        log_error "STDERR: "
        cat $err_log >&2
        log_error ""

        exit $code
    end

    echo ""
end
