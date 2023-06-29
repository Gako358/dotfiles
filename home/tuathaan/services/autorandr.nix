{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.dockingStation;
in {
  options = {
    services.dockingStation = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the autorandr service.";
      };
    };
  };
  config = mkIf (cfg.enable) {
    programs.autorandr = {
      enable = true;
      hooks.postswitch = {
        "reload-wallpaper.service" = "systemctl --user restart wallpaper.service";
      };
    };
  };
}
