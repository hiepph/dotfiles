function ,spawn --description "Spawn a process, disowned by the shell"
    command $argv[1..-1] & disown
end
