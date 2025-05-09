{ lib
, pkgs
, config
, osConfig
, inputs
, ...
}:
let
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
  lock = "${pkgs.systemd}/bin/systemctl suspend";

  cfg = config.service.hypridle;
in
{
  options.service.hypridle = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable hypridle";
    };
    dpms = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable dpms, monitor off";
    };
    timeout = lib.mkOption {
      type = lib.types.int;
      default = 3600;
      description = "Idle timeout in seconds before DPMS off and suspend actions.";
    };
    suspend = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable suspend";
    };
    suspendTimer = lib.mkOption {
      type = lib.types.int;
      default = 300;
      description = "Idle timeout in seconds before suspend actions.";
    };
  };

  config = lib.mkIf (cfg.enable && osConfig.environment.desktop.windowManager == "hyprland") {
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
        listener =
          (lib.optional cfg.dpms {
            inherit (cfg) timeout;
            on-timeout = "${hyprctl} dispatch dpms off";
            on-resume = "${hyprctl} dispatch dpms on";
          })
          ++ (lib.optional cfg.suspend {
            timeout = cfg.timeout + cfg.suspendTimer;
            on-timeout = "${lock}";
          });
      };
    };
  };
}
