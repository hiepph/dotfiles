function ,compact --description "Clear all leading, trailing spaces at the beginning, the end and the middle of the string"
    function cmd
        tr -s ' ' | tr -s '\n' | tr -s '\t' | \
            sed  -e 's/^[ \t\n]*//' -e 's/[ \t\n]*$//'
    end

    if test $argv[1]
        echo $argv[1] | cmd
    else
        read -z | cmd
    end
end
