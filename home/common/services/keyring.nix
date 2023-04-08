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
  config = mkIf (cfg.environment == "dwm") {
    services.gnome-keyring = {
      enable = true;
    };
  };
}
