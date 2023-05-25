{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  options.desktop.applet = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable the network applet.";
    };
  };
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    services.network-manager-applet = {
      enable = true;
    };
  };
}
