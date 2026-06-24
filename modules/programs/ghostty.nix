_: {
  flake.homeModules.programs-ghostty =
    {
      osConfig,
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (config.colorScheme) palette;
      cfg = config.program.ghostty;
    in
    {
      options.program.ghostty = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable ghostty terminal";
        };
        fontSize = lib.mkOption {
          type = lib.types.int;
          default = 10;
          description = "Font size for ghostty";
        };
      };

      config = lib.mkIf (cfg.enable && osConfig.environment.desktop.windowManager == "hyprland") {
        programs.ghostty = {
          enable = true;
          enableFishIntegration = true;
          settings = {
            font-family = "FiraCode";
            font-style = "Medium";
            font-family-bold = "FiraCode";
            font-style-bold = "Bold";
            font-family-italic = "JetBrainsMono Nerd Font";
            font-style-italic = "Italic";
            font-family-bold-italic = "JetBrainsMonoNL Nerd Font";
            font-style-bold-italic = "Bold Italic";
            font-size = cfg.fontSize;

            background = "#${palette.base00}";
            foreground = "#${palette.base05}";
            cursor-color = "#${palette.base05}";
            cursor-text = "#${palette.base06}";

            palette = [
              "0=#${palette.base00}"
              "1=#${palette.base08}"
              "2=#${palette.base0B}"
              "3=#${palette.base0A}"
              "4=#${palette.base0D}"
              "5=#${palette.base0E}"
              "6=#${palette.base0C}"
              "7=#${palette.base05}"
              "8=#${palette.base01}"
              "9=#${palette.base08}"
              "10=#${palette.base0B}"
              "11=#${palette.base0A}"
              "12=#${palette.base0D}"
              "13=#${palette.base0E}"
              "14=#${palette.base0C}"
              "15=#${palette.base07}"
            ];

            copy-on-select = "clipboard";
            command = "${pkgs.fish}/bin/fish";
            shell-integration = "fish";

            window-decoration = false;
            background-opacity = 0.91;
            window-padding-x = 5;
            window-padding-y = 5;
            window-width = 154;
            window-height = 37;
          };
        };
      };
    };
}
