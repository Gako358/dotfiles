{ lib
, pkgs
, inputs
, config
, osConfig
, ...
}:
let
  wallpaper = "${config.home.homeDirectory}/Sources/archive/images/wallpapers/dark_moon.jpg";
in
{
  services.hyprpaper = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
    enable = true;
    package = inputs.hyprpaper.packages.${pkgs.system}.default;

    settings = {
      preload = [ "${wallpaper}" ];
      wallpaper = [ ", ${wallpaper}" ];
    };
  };
}
