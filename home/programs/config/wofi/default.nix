{
  pkgs,
  specialArgs,
  ...
}: let
  opacity = "1";
  palette = {
    font = "RobotoMono Nerd Font";
    fontsize = "15";
    primary_accent = "cba6f7";
    secondary_accent = "89b4fa";
    tertiary_accent = "f5f5f5";
    background = "282c34";
    cursor = "Numix-Cursor";

    primary_accent_hex = "cba6f7";
    secondary_accent_hex = "89b4fa";
    tertiary_accent_hex = "f5f5f5";
    primary_background_hex = "11111B";
    secondary_background_hex = "1b1b2b";
    tertiary_background_hex = "21252b";

    primary_accent_rgba = "rgba(203,166,247,${opacity})";
    secondary_accent_rgba = "rgba(137,180,250,${opacity})";
    tertiary_accent_rgba = "rgba(245,245,245,${opacity})";
    primary_background_rgba = "rgba(40, 44, 52,${opacity})";
    secondary_background_rgba = "rgba(59, 63, 76,${opacity})";
    tertiary_background_rgba = "rgba(33, 37, 43,${opacity})";
  };
in
  if !specialArgs.desktop
  then {
    programs.wofi = {
      enable = true;
      package = pkgs.wofi.overrideAttrs (oa: {
        patches =
          (oa.patches or [])
          ++ [
            ./wofi-run-shell.patch # Fix for https://todo.sr.ht/~scoopta/wofi/174
          ];
      });
      settings = {
        allow_images = true;
        width = "25%";
        hide_scroll = true;
        term = "foot";
      };
      style = ''
        * {
          font-family: ${palette.font},monospace;
          font-weight: bold;
        }
        #window {
          border-radius: 40px;
          background: ${palette.primary_background_rgba}
        }
        #input {
          border-radius: 100px;
          margin: 20px;
          padding: 15px 25px;
          background: ${palette.tertiary_background_rgba};
          color: #${palette.tertiary_accent};
        }
        #outer-box {
          font-weight: bold;
          font-size: ${palette.fontsize};
        }
        #entry {
          margin: 10px 80px;
          padding: 20px 20px;
          border-radius: 200px;
        }
        #entry:selected{
          background-color:#${palette.primary_accent};
          color: #${palette.background};
        }
        #entry:hover {
        }
      '';
    };
  }
  else {}
