#!/bin/bash
# Get the current battery level and color the output accordingly.
INSTANCE="BAT0"
ALERT_LOW=10

bat_stats=$(upower -i /org/freedesktop/UPower/devices/battery_$INSTANCE |
    sed -e "s/ //g" |
    awk -F: 'BEGIN { ORS=" " }; /state|percentage/ { print $2 }')
state=$(echo $bat_stats | cut -d" " -f1)
percentage=$(echo $bat_stats | cut -d" " -f2 | sed -e "s/%//g")

# Icon to indicate the current charge
if [[ "$state" == "charging" ]]; then
    label=""
elif [[ "$percentage" == 100 ]]; then
    label=""
elif [[ "$percentage" > 99 ]]; then
    label=""
elif [[ "$percentage" > 75 ]]; then
    label=""
elif [[ "$percentage" > 50 ]]; then
    label=""
elif [[ "$percentage" > 25 ]]; then
    label=""
elif [[ "$percentage" > 10 ]]; then
    label=""
else
    label=""
fi

# Color based on state
if [[ $percentage == "100" ]]; then
    echo -n "<span font='12' foreground='#ebdbb2'> $label $percentage% </span>"
elif [[ $state == "charging" ]]; then
    echo -n "<span font='12' foreground='#d3869b'> $label $percentage% </span>"
elif [[ "$percentage" < "$ALERT_LOW" ]]; then
    echo -n "<span font='12' foreground='#fb4934'> $label $percentage% </span>"
elif [[ $percentage < 26 ]]; then
    echo -n "<span font='12' foreground='#fe8019'> $label $percentage% </span>"
else
    echo -n "<span font='12' foreground='#b8bb26'> $label $percentage% </span>"
fi
