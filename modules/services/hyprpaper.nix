{ lib
, pkgs
, inputs
, config
, ...
}:
let
  wallpaper = "${config.home.homeDirectory}/Sources/archive/images/wallpapers/dark_moon.jpg";
in
{
  services.hyprpaper = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    enable = true;
    package = inputs.hyprpaper.packages.${pkgs.system}.default;

    settings = {
      preload = [ "${wallpaper}" ];
      wallpaper = [ ", ${wallpaper}" ];
    };
  };
}
