{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in
{
  options.desktop.applet = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable the network applet.";
    };
  };
  config = mkIf (cfg.environment == "dwm") {
    services.network-manager-applet = {
      enable = true;
    };
  };
}
