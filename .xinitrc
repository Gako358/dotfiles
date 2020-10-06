#!/bin/sh

#Xrandr screen settings
xrandr --output DP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x0 --rotate normal

# set the background
if [ -f /usr/bin/feh ]; then
    # feh --bg-fill /home/merrinx/Pictures/Wallpapers/xinerama_landscape.jpg &
    feh --bg-scale --no-xinerama /home/merrinx/Pictures/Wallpapers/xinerama_science.jpg &
fi

# enable window compositing
# This have some errors when running monitors with different refresh rate
if [ -f /usr/bin/picom ]; then
	DISPLAY=":0" picom -b & 
fi

xset s 0 -dpms

# Start slstatus bar
dwmblocks &

# Gnome keyring
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Autostart Apps
mopidy &
brave &
sleep 3 && discord &
sleep 7 && mailspring &
# ckb-next --background &
# sleep 5 && conky --config=/home/merrinx/.config/conky/conkyrc &
# sleep 5 && conky --config=/home/merrinx/.config/conky/conkyrc2 &

# launch dwm
exec dwm

