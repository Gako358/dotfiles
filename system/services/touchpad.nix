{ config
, lib
, ...
}:
with lib;
let
  cfg = config.service.touchpad;
in
{
  options.service.touchpad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable libinput";
    };
  };

  config = mkIf cfg.enable {
    services.libinput.enable = true;
  };
}
