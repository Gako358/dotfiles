{ lib
, pkgs
, config
, inputs
, ...
}:
with lib;
let
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  lock = "${pkgs.systemd}/bin/systemctl suspend";

  cfg = config.service.hypridle;
in
{
  options.service.hypridle = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable hypridle";
    };
    timeout = mkOption {
      type = types.int;
      default = 3600;
      description = "Idle timeout in seconds before DPMS off and suspend actions.";
    };
    suspend = mkOption {
      type = types.int;
      default = 300;
      description = "Idle timeout in seconds before suspend actions.";
    };
  };

  config = mkIf cfg.enable {
    services.hypridle = {
      enable = true;
      package = inputs.hypridle.packages.${pkgs.system}.hypridle;

      settings = {
        general = {
          before_sleep_cmd = "${pkgs.systemd}/bin/loginctl lock-session";
          after-sleep-cmd = "${hyprctl} dispatch dpms on";
          lock_cmd = "pgrep hyprlock || ${lib.getExe config.programs.hyprlock.package}";
          ignore_dbus_inhibit = true;
        };
        listener = [
          {
            timeout = cfg.timeout;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
          {
            timeout = cfg.timeout + cfg.suspend;
            on-timeout = "${lock}";
          }
        ];
      };
    };
  };
}
