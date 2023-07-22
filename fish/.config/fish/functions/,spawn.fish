function ,spawn --description "Spawn a process, disowned by the shell"
    $argv[1..-1] & disown
enD
