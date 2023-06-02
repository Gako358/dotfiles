{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.dbus;
in {
  options.dbus = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable dbus service";
    };
  };

  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    services.dbus = {
      enable = true;
    };
  };
}
