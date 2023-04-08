{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; let
  colors = rec {
    background = "#312f2f";
    background-alt = "#3b4354";

    foreground = "#dcd7ba";

    primary = "#2D4F67"; # blue
    secondary = "#6A9589"; # green blue
    alert = "#E82424"; # red
    disabled = "#717C7C";

    # rofi
    bg0 = "${colors.background}E6";
    bg1 = "${colors.background-alt}80";
    bg2 = "${colors.primary}E6";
    fg0 = "${colors.foreground}";
    fg1 = "${colors.foreground}";
    fg2 = "${colors.disabled}80";
  };
  cfg = config.desktop;
in
{
  config = mkIf (cfg.environment == "dwm") {
    programs.rofi = {
      enable = true;
      font = "JetBrainsMono Nerd Font 15";
      package = with pkgs; rofi.override { plugins = [ rofi-calc rofi-emoji ]; };
      extraConfig = {
        show-icons = true;
        modi = "drun,emoji,calc";
      };
      theme =
        let
          mkL = config.lib.formats.rasi.mkLiteral;
        in
        {
          "*" = {
            bg0 = mkL colors.bg0;
            bg1 = mkL colors.bg1;
            bg2 = mkL colors.bg2;
            fg0 = mkL colors.fg0;
            fg1 = mkL colors.fg1;
            fg2 = mkL colors.fg2;

            background-color = mkL "transparent";
            text-color = mkL "@fg0";

            margin = 0;
            padding = 0;
            spacing = 0;
          };

          window = {
            background-color = mkL "@bg0";
            location = mkL "center";
            width = 640;
            border-radius = 8;
          };

          inputbar = {
            padding = mkL "12px";
            spacing = mkL "12px";
            children = map mkL [ "icon-search" "entry" ];
          };

          icon-search = {
            expand = false;
            filename = "search";
            size = mkL "28px";
            vertical-align = mkL "0.5";
          };

          entry = {
            placeholder = "Search";
            placeholder-color = mkL "@fg2";
            vertical-align = mkL "0.5";
          };

          message = {
            border = mkL "2px 0 0";
            border-color = mkL "@bg1";
            background-color = mkL "@bg1";
          };

          textbox = {
            padding = mkL "8px 24px";
          };

          listview = {
            lines = 10;
            columns = 1;
            fixed-height = false;
            border = mkL "1px 0 0";
            border-color = mkL "@bg1";
          };

          element = {
            padding = mkL "8px 16px";
            spacing = mkL "16px";
            background-color = mkL "transparent";
          };

          element-icon = {
            size = mkL "1em";
            vertical-align = mkL "0.5";
          };

          element-text = {
            text-color = mkL "inherit";
            vertical-align = mkL "0.5";
          };

          "element normal active" = {
            text-color = mkL "@bg2";
          };

          "element selected normal" = {
            background-color = mkL "@bg2";
            text-color = mkL "@fg1";
          };

          "element selected active" = {
            background-color = mkL "@bg2";
            text-color = mkL "@fg1";
          };
        };
    };
  };
}
