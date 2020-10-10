#!/bin/bash
# -------------------------------------
# Bootsrap the start of a qtile session
# >> This get's run on restart as well!
# -------------------------------------

is_running() {
    ps -aux | awk "!/grep/ && /$1/" 
}

# Set the background image
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/abstract_face.jpg &
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/abstract_earth.jpg &
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/digital_sky.jpg &
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/digital_forrest.jpg &
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/digital_streets.jpg &
# feh --bg-scale --no-xinerama /home/merrinx/Pictures/Wallpapers/xinerama_earth.jpg &
feh --bg-scale --no-xinerama /home/merrinx/Pictures/Wallpapers/xinerama_landscape.jpg &

# Wait to let the X-Session start up correctly
sleep 1

# Compton visual compositing but not for qtile as it messes things up
if ! [[ $RUNNING_QTILE ]]; then
  [[ $(is_running 'picom') ]] || picom -b -d :0 &
fi;

# Network manager
[[ $(is_running 'nm-applet') ]] || nm-applet &

# Icue Corsair
/usr/bin/ckb-next --background &
