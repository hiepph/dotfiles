# Key bindings
xbindkeys

# Ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx

# Noti
dunst &

# Display
xrandr --output eDP1 --auto --output HDMI1 --auto --right-of eDP1

# Light
xrandr --output eDP1 --brightness 1
pkill redshift; redshift -t 5000:4500 &

# Wallpaper
feh --bg-fill ~/Pictures/Wallpapers/onodera.jpg

# alttab
killall alttab; alttab -mk Super_L -bg '#c3c6b7' -fg '#000000' -t 100x100 -i 16x16 -s 2 -d 1

# Main WM
exec i3
