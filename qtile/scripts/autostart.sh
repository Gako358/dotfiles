#!/bin/bash
# -------------------------------------
# Bootsrap the start of a qtile session
# >> This get's run on restart as well!
# -------------------------------------

is_running() {
    ps -aux | awk "!/grep/ && /$1/" 
}

# Set Monitor posistion
xrandr --output DVI-D-0 --off --output HDMI-0 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-0 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off

# Set the background image
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/abstract_face.jpg &
# feh --bg-fill /home/merrinx/Pictures/Wallpapers/digital_sky.jpg &
feh --bg-fill /home/merrinx/Pictures/Wallpapers/digital_forrest.jpg &

# Wait to let the X-Session start up correctly
sleep 1

# Compton visual compositing but not for qtile as it messes things up
if ! [[ $RUNNING_QTILE ]]; then
  [[ $(is_running 'picom') ]] || picom -CG &
fi;

# Network manager
[[ $(is_running 'nm-applet') ]] || nm-applet &

# Applications
run ckb-next &
