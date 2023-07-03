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
  };
}
