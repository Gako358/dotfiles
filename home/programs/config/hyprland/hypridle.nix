{
  specialArgs,
  pkgs,
  lib,
  ...
}: let
  brillo = lib.getExe pkgs.brillo;
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  hyprlock = "${pkgs.hyprlock}/bin/hyprlock";
  timeout =
    if specialArgs.hidpi
    then 1800
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
      };

      # Lock screen on 30/10 min, screen off + 3 min
      # Suspend at 1 hour
      listener = [
        {
          timeout =
            if specialArgs.hidpi
            then null
            else timeout - 10;
          on-timeout =
            if specialArgs.hidpi
            then null
            else "${brillo} -O; ${brillo} -u 1000000 -S 10";
          on-resume =
            if specialArgs.hidpi
            then null
            else "${brillo} -I -u 500000";
        }
        {
          inherit timeout;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
        {
          timeout = timeout + 60;
          on-timeout = "${pkgs.systemd}/bin/systemctl suspend";
        }
      ];
    };
  };
}
