#!/usr/bin/env fish

function ,throw --description "Log an error message to the stderr, and then exit"
    echo $argv[1..] >&2
    exit 1
end
