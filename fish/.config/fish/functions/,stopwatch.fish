function ,stopwatch --description "Start a stopwatch"
    set start (date +%s)
    while true
        set time (math (date +%s) - $start)
        printf '%s\r ' (date -r $time +"%H:%M:%S")
        sleep .5
    end
end
