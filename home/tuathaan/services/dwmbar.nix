{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  script = pkgs.writeShellScript "dwmstatus" ''
     export PATH=${lib.makeBinPath [
      pkgs.bash
      pkgs.coreutils
      pkgs.alsa-utils
      pkgs.gawk
      pkgs.acpilight
      pkgs.gnused
      pkgs.gnugrep
      pkgs.xorg.setxkbmap
      pkgs.xorg.xsetroot
      pkgs.glibc
      pkgs.toybox
      pkgs.networkmanager
    ]}

     interval=0

     # colors
     background=#282828
     forground=#D8DEE9
     green=#A3BE8C
     grey=#3c3836
     blue=#81A1C1
     red=#BF616A
     purple=#B48EAD

     spacer() {
       printf "^b$background^ "
     }

     volume() {
       volume=$(awk -F"[][]" '/Left:/ { print $2 }' <(amixer sget Master))
       printf "^c$purple^ ^b$background^  "
       printf "^c$forground^ ^b$background^ $volume%"
     }

     cpu() {
       cpu_val=$(grep -o "^[^ ]*" /proc/loadavg)
       printf "^c$blue^ ^b$grey^  "
       printf "^c$forground^ ^b$grey^ $cpu_val"
     }

     battery() {
       get_capacity="$(cat /sys/class/power_supply/BAT0/capacity)"
       if [ "$get_capacity" -le 10 ]; then
         printf "^c$red^ ^b$grey^   "
         dunstify -u critical "Battery low" "Plug in the charger"
       elif [ "$get_capacity" -le 30 ]; then
         printf "^c$red^ ^b$grey^   "
       elif [ "$get_capacity" -le 50 ]; then
         printf "^c$blue^ ^b$grey^   "
       elif [ "$get_capacity" -le 70 ]; then
         printf "^c$blue^ ^b$grey^   "
       elif [ "$get_capacity" -le 90 ]; then
         printf "^c$blue^ ^b$grey^   "
       else
         printf "^c$blue^ ^b$grey^   "
       fi
       printf "^c$forground^ ^b$grey^$get_capacity"
     }

     mem() {
       printf "^c$blue^^b$grey^  "
       printf "^c$forground^ $(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g) "
     }

     disk_rem() {
       usage=$(df -h / | awk 'NR==2 {print $5}')
       printf "^c$blue^ ^b$grey^  "
       printf "^c$forground^ ^b$grey^ $usage"
     }

     disk_usage() {
       space_left=$(df -h / | awk 'NR==2 {print $4}')
       printf "^c$blue^ ^b$grey^  "
       printf "^c$forground^ ^b$grey^ $space_left"
     }

     brightness() {
       printf "^c$blue^^b$grey^   "
       printf "^c$forground^%.0f\n" $(xbacklight -get | sed 's/\..*//g')
     }

     # wlan() {
     #   ssid="$(LANG=C nmcli -t -f active,ssid dev wifi | grep ^yes | cut -d: -f2-)"
     # 	case "$(cat /sys/class/net/wl*/operstate 2>/dev/null)" in
     # 	up) printf "^c$green^ ^b$grey^  ^d^%s" " ^c$forground^$ssid" ;;
     # 	down) printf "^c$red^ ^b$grey^ ﲁ ^d^%s" " ^c$forground^Disconnected" ;;
     # 	esac
     # }

     wlan() {
       ssid="$(LANG=C nmcli -t -f active,ssid dev wifi | grep ^yes | cut -d: -f2-)"
       printf "^c$green^ ^b$grey^ "
       printf "^c$forground^ ^b$grey^ $ssid"
     }

     lang() {
        lang_val=$(setxkbmap -query | grep layout | awk '{print $2}')
        printf "^c$green^ ^b$background^  "
        printf "^c$forground^ ^b$background^ $lang_val "
     }

    clock() {
      printf "^c$background^ ^b$purple^   "
      printf "^c$background^ ^b$purple^ $(date '+%H:%M')"
      printf "^c$background^ ^b$purple^ $(date '+%d.%m.%Y')  "
     }

     while true; do
       sleep 1 && xsetroot -name "$(volume) $(spacer) $(spacer) $(cpu) $(battery) $(mem) $(brightness) $(spacer) $(disk_rem) $(disk_usage) $(spacer)  $(wlan) $(spacer) $(clock) $(spacer) $(lang)"
     done
  '';
in {
  config = mkIf (config.desktop.environment == "dwm") {
    systemd.user.services.dwmstatus = {
      Install.WantedBy = ["graphical-session.target"];
      Service.ExecStart = "${script}";
    };
  };
}
