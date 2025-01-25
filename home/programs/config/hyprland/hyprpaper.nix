{pkgs, ...}: let
  wallpaper = "/home/merrinx/Sources/archive/images/wallpapers/dark_moon.jpg";
in {
  home.packages = with pkgs; [
    hyprpaper
  ];

  xdg.configFile."hypr/hyprpaper.conf" = {
    text = ''
      preload = ${wallpaper}
      wallpaper = ,${wallpaper}
    '';
  };
}
