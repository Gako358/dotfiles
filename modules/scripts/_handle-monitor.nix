{ pkgs, ... }:
let
  socat = "${pkgs.socat}/bin/socat";
in
pkgs.writeShellScriptBin "handle-monitor" ''
  handle() {
    case $1 in monitoradded*)
      hyprctl dispatch 'hl.dsp.workspace.move({ workspace = "1", monitor = "1" })'
      hyprctl dispatch 'hl.dsp.workspace.move({ workspace = "2", monitor = "1" })'
      hyprctl dispatch 'hl.dsp.workspace.move({ workspace = "4", monitor = "1" })'
      hyprctl dispatch 'hl.dsp.workspace.move({ workspace = "7", monitor = "1" })'
      hyprctl dispatch 'hl.dsp.workspace.move({ workspace = "9", monitor = "1" })'
    esac
  }

  ${socat} - "UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -r line; do handle "$line"; done
''
