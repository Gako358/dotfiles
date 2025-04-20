{ osConfig
, config
, pkgs
, lib
, ...
}:
let
  commonSettings = {
    font = "RobotoMono Nerd Font";
    fontsize = "15";
    cursor = "Numix-Cursor";
  };
in
{
  programs.wofi = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
    enable = true;
    package = pkgs.wofi.overrideAttrs (oa: {
      patches =
        (oa.patches or [ ])
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
        font-family: ${commonSettings.font}, monospace;
        font-weight: bold;
        color: #${config.colorScheme.palette.base05};
      }

      #window {
        border-radius: 40px;
        background-color: #${config.colorScheme.palette.base00};
        border: 2px solid #${config.colorScheme.palette.base0D};
      }

      #input {
        border-radius: 100px;
        margin: 20px;
        padding: 15px 25px;
        background-color: #${config.colorScheme.palette.base01};
        color: #${config.colorScheme.palette.base07};
        border: none;
      }

      #input:focus {
         border: 1px solid #${config.colorScheme.palette.base0D};
      }

      #outer-box {
        font-weight: bold;
        font-size: ${commonSettings.fontsize}px;
        margin: 5px;
        padding: 10px;
      }

      #text {
         color: #${config.colorScheme.palette.base05};
      }

      #entry {
        margin: 5px 40px;
        padding: 10px 15px;
        border-radius: 100px;
        border: none;
        background-color: transparent;
      }

      #entry:selected {
        background-color: #${config.colorScheme.palette.base0D};
        color: #${config.colorScheme.palette.base00};
        border: none;
      }

      #entry:hover {
         background-color: #${config.colorScheme.palette.base02};
         color: #${config.colorScheme.palette.base06};
      }
    '';
  };
}
