{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.dbus;
in {
  options.services = {
    dbus = {
      enableModule = mkOption {
        type = types.bool;
        default = true;
        description = "Enable dbus service";
      };
    };
  };
  config = mkIf (cfg.enableModule && config.desktop.environment == "dwm") {
    services.dbus = {
      enable = true;
    };
  };
}
