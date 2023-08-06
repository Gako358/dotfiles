{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.bluethooth;
in {
  options.services = {
    bluethooth = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable bluethooth services";
      };
    };
  };

  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    services = {
      blueman.enable = true;
    };

    systemd.services = {
      usb-power-management = {
        description = "Enable USB power management";
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "oneshot";
        };
        unitConfig.RequireMountsFor = ["/sys"];
        script = ''
          echo -1 > /sys/module/usbcore/parameters/autosuspend
        '';
      };
    };
  };
}
