{pkgs, ...}: let
  xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
in
  pkgs.writeShellScriptBin "monitor" ''
    monitors=$(${xrandr} --listmonitors)

    if [[ $monitors == *"DisplayPort-2"* ]]; then
      echo "DisplayPort-2"
    else
      echo "eDP1"
    fi
  ''
