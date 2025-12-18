{ lib
, pkgs
, inputs
, osConfig
, ...
}:
let
  inherit (osConfig.environment) desktop;
in
{
  services.hyprpaper = lib.mkIf (desktop.windowManager == "hyprland") {
    enable = true;
    package = inputs.hyprpaper.packages.${pkgs.stdenv.hostPlatform.system}.default;

    settings = {
      preload = [ "${desktop.theme.wallpaper}" ];
      wallpaper = [ ", ${desktop.theme.wallpaper}" ];
    };
  };
}
