{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in
{
  config = mkIf (cfg.environment == "dwm") {
    services.picom = {
      enable = true;

      fade = true;
      fadeSteps = [ 0.03 0.03 ];
      fadeDelta = 5;

      backend = "glx";

      opacityRules = [
        "82:class_g = 'St' && !focused"
        "100:class_g = 'firefox'"
        "100:class_g = 'Gimp'"
        "100:class_g = 'thunderbird'"
        "100:class_g = 'discord'"
        "100:class_g = 'Pcmanfm'"
        "100:class_g = 'Zathura'"
        "100:window_type = 'normal'"
        "95:window_type = 'dialog'"
        "95:window_type = 'popup_menu'"
        "99:window_type = 'notification'"
      ];

      settings = {
        shadow-radius = 12;
        shadow-offset-x = -12;
        shadow-offset-y = -12;
        shadow-color = "#000000";

        no-fading-openclose = false;
        no-fading-destroyed-argb = true;

        blur = {
          method = "dual_kawase";
          strength = 7;
          deviation = 9;
          background = false;
          background-frame = false;
          background-fixed = false;
          kernel = "11x11gaussian";
          kern = "5x5box";

          background-exclude = [
            "class_g = 'firefox'"
            "class_g = 'Gimp'"
            "class_g = 'thunderbird'"
            "class_g = 'discord'"
            "class_g = 'Pcmanfm'"
            "class_g = 'Zathura'"
          ];
        };
      };
      vSync = true;
    };
  };
}
