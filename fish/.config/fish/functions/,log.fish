#!/usr/bin/env fish

function ,log --description "Log a message to the stderr"
    echo -e "\t> $argv[1..]" >&2
end
