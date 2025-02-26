let
  tmpScreenshot = "/tmp/current_background.png";
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
          path = tmpScreenshot;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "300, 50";
          outline_thickness = 1;
          fade_on_empty = true;
          dots_spacing = 0.2;
          dots_center = true;
          position = "0, -120";
        }
      ];
    };
  };
}
