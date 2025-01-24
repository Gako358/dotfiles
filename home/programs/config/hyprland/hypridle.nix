{pkgs, ...}: let
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  hyprlock = "${pkgs.hyprlock}/bin/hyprlock";
  timeout = 3600;
in {
  services.hypridle = {
    enable = true;

    settings = {
      general = {
        lock_cmd = "pidof ${hyprlock} || ${hyprlock}";
        before_sleep_cmd = "${pkgs.systemd}/bin/loginctl lock-session";
        after-sleep-cmd = "${hyprctl} dispatch dpms on";
        ignore_dbus_inhibit = true;
      };

      # Lock screen on 60/10 min, screen off + 3 min
      # Suspend at 1 hour
      listener = [
        {
          timeout = timeout;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
        {
          timeout =
            timeout + 370;
          on-timeout = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };
}
