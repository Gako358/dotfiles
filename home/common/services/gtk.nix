{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.gtk;
in {
  options = {
    services.gtk = {
      enable = mkOption {
        default = true;
        description = "Enable gtk theme and icon theme";
        type = types.bool;
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm" || config.desktop.environment == "bspwm")) {
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
