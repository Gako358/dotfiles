{
  pkgs,
  ...
}: let
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  hyprlock = "${pkgs.hyprlock}/bin/hyprlock";
in {
  # screen idle
  services.hypridle = {
    enable = true;

    settings = {
      general = {
        lock_cmd = "pidof ${hyprlock} || ${hyprlock}";
        before_sleep_cmd = "${pkgs.systemd}/bin/loginctl lock-session";
        after-sleep-cmd = "${hyprctl} dispatch dpms on";
        ignore_dbus_inhibit = true;
      };

      # Lock screen on 30/10 min, screen off + 3 min
      # Suspend at 1 hour
      listener = [
        {
          timeout = null;
          on-timeout = null;
          on-resume = null;
        }
        {
          timeout = null;
          on-timeout = null;
        }
      ];
    };
  };
}
