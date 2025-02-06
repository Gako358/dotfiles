{pkgs, ...}: let
  figlet = "${pkgs.figlet}/bin/figlet";
in
  pkgs.writeShellScriptBin "countdown-timer" ''
    display_time() {
      clear
      printf "Counting down from %s seconds\n" "$2" | ${figlet}
      echo ""
      echo ""
      echo "$1 seconds" | ${figlet}
    }

    countdown() {
      local seconds=$1
      local total_seconds=$1
      while [ $seconds -ge 0 ]; do
        display_time $seconds $total_seconds
        sleep 1
        seconds=$((seconds - 1))
      done
      clear
      echo "Time is up" | ${figlet}
    }

    if [ -z "$1" ]; then
      echo "Usage: countdown-timer <seconds>"
      exit 1
    fi

    countdown $1
  ''
