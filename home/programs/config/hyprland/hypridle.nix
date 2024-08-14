{
  specialArgs,
  pkgs,
  ...
}: let
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  hyprlock = "${pkgs.hyprlock}/bin/hyprlock";
  timeout =
    if specialArgs.hidpi
    then 1900
    else 600;
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
          timeout =
            if specialArgs.hidpi
            then timeout
            else null;
          on-timeout =
            if specialArgs.hidpi
            then "hyprctl dispatch dpms off"
            else null;
          on-resume =
            if specialArgs.hidpi
            then "hyprctl dispatch dpms on"
            else null;
        }
        {
          timeout =
            if specialArgs.hidpi
            then timeout + 370
            else null;
          on-timeout =
            if specialArgs.hidpi
            then "${pkgs.systemd}/bin/systemctl suspend"
            else null;
        }
      ];
    };
  };
}
