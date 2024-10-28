{pkgs, ...}: let
  fontSize = 10;
in {
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
          background = "#282828";
          foreground = "#c5c8c6";
        };
        cursor = {
          text = "#cdcecf";
          cursor = "#aeafb0";
        };
        vi_mode_cursor = {
          text = "#cdcecf";
          cursor = "#63cdcf";
        };
        search = {
          matches = {
            foreground = "#cdcecf";
            background = "#3c5372";
          };
          focused_match = {
            foreground = "#cdcecf";
            background = "#81b29a";
          };
        };
        footer_bar = {
          foreground = "#cdcecf";
          background = "#29394f";
        };
        hints = {
          start = {
            foreground = "#cdcecf";
            background = "#f4a261";
          };
          end = {
            foreground = "#cdcecf";
            background = "#29394f";
          };
        };
        normal = {
          black = "#393b44";
          red = "#c94f6d";
          green = "#81b29a";
          yellow = "#dbc074";
          blue = "#719cd6";
          magenta = "#9d79d6";
          cyan = "#63cdcf";
          white = "#dfdfe0";
        };
        bright = {
          black = "#575860";
          red = "#d16983";
          green = "#8ebaa4";
          yellow = "#e0c989";
          blue = "#86abdc";
          magenta = "#baa1e2";
          cyan = "#7ad5d6";
          white = "#e4e4e5";
        };
        dim = {
          black = "#30323a";
          red = "#ab435d";
          green = "#6e9783";
          yellow = "#baa363";
          blue = "#6085b6";
          magenta = "#8567b6";
          cyan = "#54aeb0";
          white = "#bebebe";
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
