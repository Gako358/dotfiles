{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    services.gnome-keyring.enable = true;
    home.packages = with pkgs; [
      gnome.seahorse
    ];
  };
}
