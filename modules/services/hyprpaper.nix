{ pkgs, inputs, ... }:
let
  wallpaper = "/home/merrinx/Sources/archive/images/wallpapers/dark_moon.jpg";
in
{
  services.hyprpaper = {
    enable = true;
    package = inputs.hyprpaper.packages.${pkgs.system}.default;

    settings = {
      preload = [ "${wallpaper}" ];
      wallpaper = [ ", ${wallpaper}" ];
    };
  };
}
