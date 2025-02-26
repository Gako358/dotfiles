{pkgs, ...}: let
  socat = "${pkgs.socat}/bin/socat";
in
  pkgs.writeShellScriptBin "handle-monitor" ''
    handle() {
      case $1 in monitoradded*)
        hyprctl dispatch moveworkspacetomonitor "1 1"
        hyprctl dispatch moveworkspacetomonitor "2 1"
        hyprctl dispatch moveworkspacetomonitor "4 1"
        hyprctl dispatch moveworkspacetomonitor "7 1"
        hyprctl dispatch moveworkspacetomonitor "9 1"
      esac
    }

    ${socat} - "UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -r line; do handle "$line"; done
  ''
