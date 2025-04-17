{ config
, lib
, ...
}:
with lib;
let
  cfg = config.service.blueman;
in
{
  options.service.blueman = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable blueman";
    };
  };

  config = mkIf cfg.enable {
    services.blueman.enable = true;
  };
}
