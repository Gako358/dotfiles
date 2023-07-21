{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.terminal.kitty;
in {
  options.programs.terminal.kitty.enable = mkEnableOption "Kitty terminal emulator";
  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
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
      settings = {
        env = {
          TERM = "xterm-256color";
        };
        crollback_lines = 10000;
        placement_strategy = "center";

        allow_remote_control = "yes";
        enable_audio_bell = "no";
        visual_bell_duration = "0.1";
        visual_bell_color = "0xdcd7ba";

        copy_on_select = "clipboard";
        background = "0x272727";
        foreground = "0xdcd7ba";
        color0 = "0x090618";
        color8 = "0x727169";
        color1 = "0xc34043";
        color9 = "0xe82424";
        color2 = "0x76946a";
        color10 = "0x98bb6c";
        color3 = "0xc0a36e";
        color11 = "0xe6c384";
        color4 = "0x7e9cd8";
        color12 = "0x7fb4ca";
        color5 = "0x957fb8";
        color13 = "0x938aa9";
        color6 = "0x6a9589";
        color14 = "0x7aa89f";
        color7 = "0xc8c093";
        color15 = "0xffffff";
      };
    };
  };
}
