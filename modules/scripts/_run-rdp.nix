{ pkgs, ... }:
pkgs.writeShellScriptBin "run-rdp" /* bash */ ''
  set -euo pipefail

  if [ "$#" -ne 1 ]; then
    echo "Usage: run-rdp FILE.rdp" >&2
    exit 2
  fi

  if [ ! -f "$1" ]; then
    echo "Error: RDP file not found: $1" >&2
    exit 1
  fi

  rdp_file=$(${pkgs.coreutils}/bin/mktemp --suffix=.rdp)
  trap '${pkgs.coreutils}/bin/rm -f "$rdp_file"' EXIT

  ${pkgs.gnused}/bin/sed \
    -e '/^smart sizing:/Id' \
    -e '/^dynamic resolution:/Id' \
    -e '/^use multimon:/Id' \
    -e '/^span monitors:/Id' \
    "$1" > "$rdp_file"

  if [ -n "''${WAYLAND_DISPLAY:-}" ]; then
    export SDL_VIDEODRIVER=wayland
    # Hyprland reports the fullscreen window as 64x64 before it is mapped, which FreeRDP rejects.
    if ${pkgs.gnugrep}/bin/grep -qi '^screen mode id:i:2' "$rdp_file"; then
      export FREERDP_WLROOTS_HACK=force
    fi
  fi

  ${pkgs.freerdp}/bin/sdl-freerdp "$rdp_file" /dynamic-resolution
''
