#!/bin/sh

#Xrandr screen settings
xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1080x840 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate right --output DP-3 --mode 1920x1080 --pos 3000x840 --rotate normal

# set the background
if [ -f /usr/bin/feh ]; then
    feh --bg-fill /home/merrinx/Pictures/Wallpapers/xinerama_landscape.jpg &
fi

# enable window compositing
if [ -f /usr/bin/picom ]; then
    picom -b -d :0 & 
fi

# launch dwm
exec dwm

# Start slstatus bar
/usr/local/bin/dwmblocks &

# Autostart Apps
chromium &
discord &
mailspring &

# relaunch DWM if the binary changes, otherwise bail
csum=$(sha1sum $(which dwm))
new_csum=""
while true
do
    if [ "$csum" != "$new_csum" ]
    then
        csum=$new_csum
        dwm
    else
        exit 0
    fi
    new_csum=$(sha1sum $(which dwm))
    sleep 0.5
done
