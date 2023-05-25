{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    gtk = {
      enable = true;
      theme = {
        name = "Flat-Remix-GTK-White-Dark";
        package = pkgs.flat-remix-gtk;
      };
      iconTheme = {
        name = "Flat-Remix-Blue-Dark";
        package = pkgs.flat-remix-icon-theme;
      };
    };
  };
}
