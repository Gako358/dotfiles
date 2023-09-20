{pkgs, ...}: let
  wallpaper = "/home/merrinx/Sources/archive/images/wallpapers/mesmerizing_night.jpg";
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
