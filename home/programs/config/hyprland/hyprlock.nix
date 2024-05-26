let
  wallpaper = "/home/merrinx/Sources/archive/images/wallpapers/dark_moon.jpg";
  font = "RobotoMono Nerd Font";
in {
  programs.hyprlock = {
    enable = true;

    settings = {
      general = {
        disable_loading_bar = true;
        hide_cursor = false;
        no_fade_in = true;
      };

      background = [
        {
          monitor = "";
          path = wallpaper;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "300, 50";
          outline_thickness = 1;
          fade_on_empty = false;
          dots_spacing = 0.2;
          dots_center = true;
        }
      ];

      label = [
        {
          monitor = "";
          text = "$TIME";
          font_family = font;
          font_size = 50;
          position = "0, 80";
          valign = "center";
          halign = "center";
        }
      ];
    };
  };
}
