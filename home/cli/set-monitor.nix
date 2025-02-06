{pkgs, ...}: let
  monitorOff = "hyprctl keyword monitor eDP-1,disable";
  monitorOn = " hyprctl keyword monitor eDP-1,1920x1080@60,0x0,1";
in
  pkgs.writeShellScriptBin "set-monitor" ''
    if [ "$1" = "off" ]; then
      ${monitorOff}
    elif [ "$1" = "on" ]; then
      ${monitorOn}
    else
      echo "Usage: set-monitor [on|off]"
    fi
  ''
