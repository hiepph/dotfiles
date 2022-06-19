function check_battery
    set bat_files "/sys/class/power_supply/BAT0"
    set bat_status (cat $bat_files/status)
    set bat_capacity (cat $bat_files/capacity)

    if test $bat_status = "Discharging" && test $bat_capacity -le 15
        notify-send -u critical "Battery low: $bat_capacity%" -t 10000
    end
end
