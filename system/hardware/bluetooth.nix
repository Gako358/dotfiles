{ config
, lib
, ...
}:
let
  cfg = config.system.bluetooth;
in
{
  options.system.bluetooth = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable bluetooth";
    };
  };

  config = lib.mkIf cfg.enable {
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
