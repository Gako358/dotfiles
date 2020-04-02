#!/bin/bash
# -------------------------------------
# Bootsrap the start of a qtile session
# >> This get's run on restart as well!
# -------------------------------------

is_running() {
    ps -aux | awk "!/grep/ && /$1/" 
}

# Set the background image
feh --bg-fill /home/merrinx/Pictures/Wallpapers/abstract_background.jpg &
# feh --bg-fill /home/innes/Pictures/Wallpapers/dunstanburgh.jpg &
# feh --bg-fill /home/innes/Pictures/Wallpapers/turtle.jpg &
# feh --bg-fill /home/innes/Pictures/Wallpapers/cookies.jpg &
# feh --bg-fill /home/innes/Pictures/Wallpapers/mbridge-fields.jpg &
# feh --bg-fill /home/innes/Pictures/Wallpapers/elephant.png &

# Wait to let the X-Session start up correctly
sleep 1

# Compton visual compositing but not for qtile as it messes things up
if ! [[ $RUNNING_QTILE ]]; then
  [[ $(is_running 'picom') ]] || picom -CG &
fi;

# Network manager
[[ $(is_running 'nm-applet') ]] || nm-applet &

