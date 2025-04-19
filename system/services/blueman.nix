{ config
, lib
, ...
}:
let
  cfg = config.service.blueman;
in
{
  options.service.blueman = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable blueman";
    };
  };

  config = lib.mkIf cfg.enable {
    services.blueman.enable = true;
  };
}
