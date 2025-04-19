{ config
, lib
, ...
}:
let
  cfg = config.service.touchpad;
in
{
  options.service.touchpad = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable libinput";
    };
  };

  config = lib.mkIf cfg.enable {
    services.libinput.enable = true;
  };
}
