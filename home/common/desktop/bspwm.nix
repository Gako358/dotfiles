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
  config = mkIf (cfg.environment == "bspwm") {
    xsession.windowManager.bspwm = {
      enable = true;
      settings = {
        border_width = 2;
        window_gap = 19;
        gapless_monocle = true;
        borderless_monocle = true;
        split_ratio = 0.55;

        focus_border_color = "#2D4F67"; # blue
      };
      # monitors = {
      #   DP-2 = [ "1" "2" "3" "4" "5" ];
      # };
      startupPrograms = [
        "firefox"
        "discord"
        "thunderbird"
        # "systemctl --user restart polybar"
      ];
      extraConfig = ''
      '';
      rules = {
        "Pcmanfm" = {
          state = "floating";
          follow = true;
        };
        "Gimp" = {
          state = "floating";
          follow = true;
        };
        "thunderbird" = {
          desktop = "^4";
          state = "floating";
          follow = false;
        };
        "firefox" = {
          desktop = "^1";
          follow = false;
        };
        "discord" = {
          desktop = "^5";
          follow = false;
        };
      };
    };
  };
}
