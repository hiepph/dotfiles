function ,sh --description "Execute a command, verbosely"
    echo -e "\t\$ $argv"
    command $argv[1..-1]
end
