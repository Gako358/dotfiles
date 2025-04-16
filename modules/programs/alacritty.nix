{ pkgs, config, ... }:
let
  fontSize = 10;
in
{
  programs.alacritty = {
    enable = true;
    settings = {
      bell = {
        animation = "EaseOutExpo";
        duration = 5;
        color = "#ffffff";
      };
      colors = {
        primary = {
          background = "#${config.colorScheme.palette.base00}";
          foreground = "#${config.colorScheme.palette.base05}";
        };
        cursor = {
          text = "#${config.colorScheme.palette.base06}";
          cursor = "#${config.colorScheme.palette.base05}";
        };
        vi_mode_cursor = {
          text = "#${config.colorScheme.palette.base06}";
          cursor = "#${config.colorScheme.palette.base0C}";
        };
        search = {
          matches = {
            foreground = "#${config.colorScheme.palette.base06}";
            background = "#${config.colorScheme.palette.base03}";
          };
          focused_match = {
            foreground = "#${config.colorScheme.palette.base06}";
            background = "#${config.colorScheme.palette.base0B}";
          };
        };
        footer_bar = {
          foreground = "#${config.colorScheme.palette.base06}";
          background = "#${config.colorScheme.palette.base01}";
        };
        hints = {
          start = {
            foreground = "#${config.colorScheme.palette.base06}";
            background = "#${config.colorScheme.palette.base09}";
          };
          end = {
            foreground = "#${config.colorScheme.palette.base06}";
            background = "#${config.colorScheme.palette.base01}";
          };
        };
        normal = {
          black = "#${config.colorScheme.palette.base00}";
          red = "#${config.colorScheme.palette.base08}";
          green = "#${config.colorScheme.palette.base0B}";
          yellow = "#${config.colorScheme.palette.base0A}";
          blue = "#${config.colorScheme.palette.base0D}";
          magenta = "#${config.colorScheme.palette.base0E}";
          cyan = "#${config.colorScheme.palette.base0C}";
          white = "#${config.colorScheme.palette.base05}";
        };
        bright = {
          black = "#${config.colorScheme.palette.base01}";
          red = "#${config.colorScheme.palette.base08}";
          green = "#${config.colorScheme.palette.base0B}";
          yellow = "#${config.colorScheme.palette.base0A}";
          blue = "#${config.colorScheme.palette.base0D}";
          magenta = "#${config.colorScheme.palette.base0E}";
          cyan = "#${config.colorScheme.palette.base0C}";
          white = "#${config.colorScheme.palette.base07}";
        };
        dim = {
          black = "#${config.colorScheme.palette.base02}";
          red = "#${config.colorScheme.palette.base08}";
          green = "#${config.colorScheme.palette.base0B}";
          yellow = "#${config.colorScheme.palette.base0A}";
          blue = "#${config.colorScheme.palette.base0D}";
          magenta = "#${config.colorScheme.palette.base0E}";
          cyan = "#${config.colorScheme.palette.base0C}";
          white = "#${config.colorScheme.palette.base04}";
        };
      };
      font = {
        normal = {
          family = "FiraCode";
          style = "Medium";
        };
        bold = {
          family = "FiraCode";
          style = "Bold";
        };
        italic = {
          family = "JetBrainsMono Nerd Font";
          style = "Italic";
        };
        bold_italic = {
          family = "JetBrainsMonoNL Nerd Font";
          style = "Bold Italic";
        };
        size = fontSize;
      };
      hints.enabled = [
        {
          regex = ''(mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-\u009F<>"\\s{-}\\^⟨⟩`]+'';
          command = "${pkgs.mimeo}/bin/mimeo";
          post_processing = true;
          mouse.enabled = true;
        }
      ];
      selection.save_to_clipboard = true;
      terminal.shell.program = "${pkgs.fish}/bin/fish";
      window = {
        dimensions = {
          columns = 154;
          lines = 37;
        };
        decorations = "none";
        opacity = 0.91;
        padding = {
          x = 5;
          y = 5;
        };
      };
    };
  };
}
