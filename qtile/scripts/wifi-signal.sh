#!/bin/bash
# wifi signal quality

# INTERFACE="wlan0"
INTERFACE="wlp61s0"
QUALITY=$(grep $INTERFACE /proc/net/wireless | awk '{ print int($3 * 100 / 70) }')

# If the machine has no battery or wireless connection, the corresponding block should not be displayed.
[[ ! -d /sys/class/net/${INTERFACE}/wireless ]] ||
    [[ "$(cat /sys/class/net/$INTERFACE/operstate)" = 'down' ]] && exit

# wifi signal bars
if [[ $QUALITY -ge 80 ]]; then
  echo -n "<span  font='11' foreground='#b8bb26'>$(spark 0 1 2 3 4) </span>"
elif [[ $QUALITY -lt 80 ]]; then
  echo -n "<span  font='11' foreground='#fabd2f'>$(spark 0 1 2 3 0) </span>"
elif [[ $QUALITY -lt 60 ]]; then
  echo -n "<span  font='11' foreground='#fe8019'>$(spark 0 1 2 0 0) </span>"
elif [[ $QUALITY -lt 40 ]]; then
  echo -n "<span  font='11' foreground='#fb4934'>$(spark 0 1 0 0 0) </span>"
fi

case $WIDGET_BUTTON in
    1) notify-send "Available Networks" "$(nmcli dev wifi)" ;;
esac
