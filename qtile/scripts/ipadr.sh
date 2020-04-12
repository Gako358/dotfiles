#!/bin/bash
# Find my current IP address

INTERFACE=$(ip route | awk '/^default/ { print $5 ; exit }')
# INTERFACE="wlp61s0"
# IP=$(dig TXT +short o-o.myaddr.l.google.com @ns1.google.com | awk -F'"' '{ print $2}')
IP=$(ip route get 1 | cut -d' ' -f7)
WANIP=$(curl -s http://whatismijnip.nl |cut -d " " -f 5)

if [[ "$(cat /sys/class/net/$INTERFACE/operstate)" = 'down' ]]; then
  echo -n "<span font='8' foreground='#83a598'> </span><span foreground='#9d0006'> X </span>"
else
  echo -n "<span font='8' foreground='#83a598'> </span><span font='8' foreground='#7c6f64'>/</span><span font='10' foreground='#ebdbb2'>$IP </span>"
fi

# mouse buttons
case $WIDGET_BUTTON in
   1) rofi-wifi-menu ;;
   2) notify-send " $INTERFACE" "ip = $IP\nwanip = $WANIP\ngateway = $(ip route | cut -d " " -f 3 | grep [.])\nroute = $(ip route | cut -d " " -f 1 | grep [0-9])\nresolv = $(cat /etc/resolv.conf | cut -d " " -f 2 | grep [0-9])" ;;
esac
