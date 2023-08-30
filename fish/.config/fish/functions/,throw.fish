#!/usr/bin/env fish

function ,throw --description "Log an error message to the stderr, and then exit"
    set_color red
    ,log --error $argv[1..]
    set_color normal
    exit 1
end
