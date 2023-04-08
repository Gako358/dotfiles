{ config
, pkgs
, host
, lib
, ...
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

     mem() {
       printf "^c$blue^^b$grey^  "
       printf "^c$forground^ $(free -h | awk '/^Mem/ { print $3 }' | sed s/i//g) "
     }

     # wlan2() {
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
       sleep 1 && xsetroot -name "$(volume) $(spacer) $(spacer) $(cpu) $(mem) $(spacer) $(wlan) $(spacer) $(clock) $(spacer) $(lang)"
     done
  '';

  cfg = config.desktop;
in
{
  config = mkIf (cfg.environment == "dwm") {
    systemd.user.services.dwmstatus = {
      Install.WantedBy = [ "graphical-session.target" ];
      Service.ExecStart = "${script}";
    };
  };
}
