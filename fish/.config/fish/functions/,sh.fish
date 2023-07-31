function ,sh --description "Execute a command, verbosely"
    echo -e "\t\$ $argv" >&2

    set tmp_dir (mktemp -d)
    set err_log $tmp_dir/,sh.err

    $argv[1..-1] 2>$err_log
    set code $status

    if test $status -ne 0
        ,throw "Command failed: \"$argv[1..-1]\" with status code: $code"

        ,throw "STDERR: "
        cat $err_log >&2
        ,throw ""

        exit $code
    end
end
