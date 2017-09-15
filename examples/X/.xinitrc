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

# Main WM
exec i3
