{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.terminal.gnome-terminal;
in {
  options.programs.terminal.gnome-terminal.enable = mkEnableOption "Gnome Terminal";
  config = lib.mkIf cfg.enable {
    programs.gnome-terminal = {
      enable = true;
      showMenubar = false;
      profile = {
        "5ddfe964-7ee6-4131-b449-26bdd97518f7" = {
          default = true;
          visibleName = "Kanagawa";
          cursorBlinkMode = "on";
          cursorShape = "ibeam";
          font = "DejaVu Sans Mono 13";
          showScrollbar = false;
          colors = {
            foregroundColor = "rgb(197,200,198)";
            palette = [
              # Normal colors
              "rgb(9,6,24)" # black
              "rgb(195,64,67)" # red
              "rgb(118,148,106)" # green
              "rgb(192,163,110)" # yellow
              "rgb(126,156,216)" # blue
              "rgb(149,127,184)" # magenta
              "rgb(106,149,137)" # cyan
              "rgb(200,192,147)" # white
              # Bright colors
              "rgb(114,113,105)" # black
              "rgb(232,36,36)" # red
              "rgb(152,187,108)" # green
              "rgb(230,195,132)" # yellow
              "rgb(127,180,202)" # blue
              "rgb(147,138,169)" # magenta
              "rgb(122,168,159)" # cyan
              "rgb(255,255,255)" # white
            ];
            boldColor = "rgb(138,186,183)";
            backgroundColor = "rgb(39,39,39)";
          };
        };
      };
    };
  };
}
