{ osConfig
, config
, pkgs
, lib
, ...
}:
let
  inherit (config.colorScheme) palette;
  cfg = config.program.foot;
in
{

  options.program.foot = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable foot terminal";
    };
    fontSize = lib.mkOption {
      type = lib.types.int;
      default = 10;
      description = "Font size for foot";
    };
  };
  config = lib.mkIf (cfg.enable && osConfig.environment.desktop.windowManager == "hyprland") {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          font = "FiraCode:size=${toString cfg.fontSize}";
          horizontal-letter-offset = 0;
          vertical-letter-offset = 0;
          pad = "5x5 center";
          selection-target = "clipboard";
        };

        bell = {
          urgent = "yes";
          notify = "yes";
        };

        desktop-notifications = {
          command = "${lib.getExe pkgs.libnotify} -a \${app-id} -i \${app-id} \${title} \${body}";
        };

        scrollback = {
          lines = 10000;
          multiplier = 3;
          indicator-position = "relative";
          indicator-format = "line";
        };

        url = {
          launch = "${pkgs.mimeo}/bin/mimeo \${url}";
        };

        cursor = {
          style = "beam";
          beam-thickness = 1;
        };

        colors = {
          alpha = 0.91;
          foreground = "${palette.base05}";
          background = "${palette.base00}";
          regular0 = "${palette.base00}"; # black
          regular1 = "${palette.base08}"; # red
          regular2 = "${palette.base0B}"; # green
          regular3 = "${palette.base0A}"; # yellow
          regular4 = "${palette.base0D}"; # blue
          regular5 = "${palette.base0E}"; # magenta
          regular6 = "${palette.base0C}"; # cyan
          regular7 = "${palette.base05}"; # white
          bright0 = "${palette.base03}"; # bright black
          bright1 = "${palette.base08}"; # bright red
          bright2 = "${palette.base0B}"; # bright green
          bright3 = "${palette.base0A}"; # bright yellow
          bright4 = "${palette.base0D}"; # bright blue
          bright5 = "${palette.base0E}"; # bright magenta
          bright6 = "${palette.base0C}"; # bright cyan
          bright7 = "${palette.base07}"; # bright white
        };
      };
    };
  };
}
