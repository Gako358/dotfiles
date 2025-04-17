{ config
, lib
, ...
}:
with lib;
let
  cfg = config.system.bluetooth;
in
{
  options.system.bluetooth = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable bluetooth";
    };
  };

  config = mkIf cfg.enable {

    environment.persistence."/persist" = {
      directories = [
        "/var/lib/bluetooth"
      ];
    };

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          Name = "Computer";
          ControllerMode = "dual";
          FastConnectable = "true";
          Experimental = "true";
        };
        Policy = { AutoEnable = "true"; };
        LE = { EnableAdvMonInterleaveScan = "true"; };
      };
    };
  };
}
