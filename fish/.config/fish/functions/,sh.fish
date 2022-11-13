function ,sh --description "Execute a command, verbosely"
    echo "\$ $argv"
    command $argv[1..-1]
    echo ""
end
