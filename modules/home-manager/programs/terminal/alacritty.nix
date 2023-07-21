{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.terminal.alacritty;
in {
  options.programs.terminal.alacritty.enable = mkEnableOption "alacritty terminal emulator";
  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        env = {
          TERM = "xterm-256color";
        };
        scrolling.history = 10000;
        scrolling.multiplier = 3;
        window = {
          opacity = 0.99;
          startup_mode = "Windowed";
          decorations = "none";
        };
        font = {
          normal = {
            family = "DejaVu Sans Mono";
            style = "Regular";
          };
          bold = {
            family = "DejaVu Sans Mono";
            style = "Bold";
          };
          italic = {
            family = "DejaVu Sans Mono";
            style = "Italic";
          };
          bold_italic = {
            family = "DejaVu Sans Mono";
            style = "Bold Italic";
          };
          size = 13;
        };
        mouse_bindings = [
          {
            mouse = "Right";
            action = "PasteSelection";
          }
        ];
        colors = {
          # Default colors
          primary = {
            background = "0x272727";
            foreground = "0xdcd7ba";
          };
          # Normal colors
          normal = {
            black = "0x090618";
            red = "0xc34043";
            green = "0x76946a";
            yellow = "0xc0a36e";
            blue = "0x7e9cd8";
            magenta = "0x957fb8";
            cyan = "0x6a9589";
            white = "0xc8c093";
          };
          # Bright colors
          bright = {
            black = "0x727169";
            red = "0xe82424";
            green = "0x98bb6c";
            yellow = "0xe6c384";
            blue = "0x7fb4ca";
            magenta = "0x938aa9";
            cyan = "0x7aa89f";
            white = "0xffffff";
          };
        };
        cursor = {
          style = {
            shape = "block";
            blinking = "always";
          };
          blink_timeout = 0;
        };
      };
    };
  };
}
