#!/bin/sh

#Xrandr screen settings
xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1080x840 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate right --output DP-3 --mode 1920x1080 --pos 3000x840 --rotate normal

# set the background
if [ -f /usr/bin/feh ]; then
    # feh --bg-fill /home/merrinx/Pictures/Wallpapers/xinerama_landscape.jpg &
    feh --bg-scale --no-xinerama /home/merrinx/Pictures/Wallpapers/xinerama_landscape.jpg &
fi

# enable window compositing
if [ -f /usr/bin/picom ]; then
	picom -b & 
fi

# Start slstatus bar
dwmblocks &

# Gnome keyring
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Autostart Apps
brave &
discord &
mailspring &
mopidy &

# launch dwm
exec dwm

