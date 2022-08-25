function ,trim --description "Clear all leading, trailing spaces at the beginning, the end and the middle of the string"
    function cmd
        sed 's/ *//' | tr -s ' '
    end

    if test $argv[1]
        echo $argv[1] | cmd
    else
        read -z | cmd
    end
end
