{ lib
, pkgs
, config
, osConfig
, ...
}:
let
  fontSize = "14px";
  iconSize = "17px";
  opacity = "0.46";
  palette = {
    font = "RobotoMono Nerd Font";
    fontsize = fontSize;
    iconsize = iconSize;
    background-color = "rgba(26, 26, 26, ${opacity})";
    background_border-frame = "#${config.colorScheme.palette.base02}";

    blue = "#${config.colorScheme.palette.base0D}";
    cyan = "#${config.colorScheme.palette.base0C}";
    green = "#${config.colorScheme.palette.base0B}";
    grey = "#${config.colorScheme.palette.base04}";
    magenta = "#${config.colorScheme.palette.base0E}";
    orange = "#${config.colorScheme.palette.base09}";
    red = "#${config.colorScheme.palette.base08}";
    yellow = "#${config.colorScheme.palette.base0A}";
  };
  calendar = "${pkgs.gnome-calendar}/bin/gnome-calendar";
  lockScreen = "${pkgs.hyprlock}/bin/hyprlock";
  system = "${pkgs.gnome-system-monitor}/bin/gnome-system-monitor";
in
{
  programs.waybar = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oa: {
      mesonFlags = (oa.mesonFlags or [ ]) ++ [ "-Dexperimental=true" ];
    });
    systemd.enable = true;
    settings.mainBar = {
      position = "top";
      layer = "top";
      height = 28;
      margin-top = 3;
      margin-bottom = 2;
      margin-left = 4;
      margin-right = 4;
      modules-left = [
        "custom/launcher"
        "hyprland/workspaces"
      ];
      modules-center = [
        "clock"
      ];
      modules-right = [
        "tray"
        "custom/space"
        "group/system"
        "custom/space"
        "custom/lock"
      ];
      battery = {
        states = {
          good = 95;
          warning = 30;
          critical = 15;
        };
        format = "{icon}";
        format-charging = "";
        format-plugged = "";
        format-alt = "{icon} {time}";
        format-icons = [ "" "" "" "" "" ];
      };

      clock = {
        format = " {:%a, %d %b, %I:%M %p}";
        tooltip = "true";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        format-alt = " {:%d/%m}";
        on-click = "${calendar}";
      };

      "custom/launcher" = {
        format = "   ";
        tooltip = false;
      };
      "custom/lock" = {
        "format" = "󰌾";
        "tooltip" = true;
        "tooltip-format" = "Lock Screen";
        "on-click" = "${lockScreen}";
      };
      "custom/power" = {
        "format" = "󰐥";
        "tooltip" = true;
        "tooltip-format" = "Power menu (wlogout)";
        "on-click" = "wlogout";
      };
      "custom/space" = {
        "format" = " ";
        "tooltip" = false;
      };

      "hyprland/workspaces" = {
        format = "{icon}";
        on-click = "activate";
        format-icons = {
          default = "";
          active = "";
        };
        sort-by-number = true;
      };

      memory = {
        format = "󰍛";
        on-click = "${system}";
        interval = 5;
      };

      network = {
        format-wifi = " ";
        format-ethernet = " ";
        tooltip-format = "Connected to {essid} {ifname} via {gwaddr}";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "󰖪 ";
      };

      pulseaudio = {
        format = "{icon}";
        format-muted = "󰝟";
        format-icons = {
          default = [ "󰕿" "󰖀" "󰕾" ];
        };
        scroll-step = 5;
        on-click = "pavucontrol";
      };

      temperature = {
        format = "";
        on-click = "${system}";
        tooltip = true;
      };

      tray = {
        icon-size = 20;
        spacing = 8;
      };

      "group/system" = {
        "orientation" = "horizontal";
        "modules" = [
          "temperature"
          "memory"
          "battery"
          "pulseaudio"
          "network"
        ];
      };
    };
    style = ''
      * {
          border: none;
          border-radius: 0px;
          font-family: ${palette.font};
          font-size: ${palette.fontsize};
          min-height: 0;
      }

      window#waybar {
         background-color: transparent;
      }
      window > box {
         margin-left: 5px;
         margin-right: 5px;
         margin-top: 5px;
         background-color: ${palette.background-color};
         border: 2px none ${palette.blue};
      }

      #workspaces button:hover,
      #workspaces button.active:hover,
      #workspaces button.empty:hover
      {
        margin: initial;
        box-shadow: initial;
      }

      #workspaces button {
        margin: initial;
        padding: initial;
        padding-left: 2px;
        padding-right: 2px;
        color: ${palette.grey};
      }

      #workspaces button.active,
      #workspaces button.empty
      {
        color: ${palette.blue};
      }

      #custom-launcher {
         font-size: 20px;
         padding-left: 16px;
         padding-right: 28px;
         color: ${palette.blue};
         padding: 2px 8px;
      }

      #custom-lock {
        padding-left: 7px;
        padding-right: 7px;
        color: ${palette.green};
        background-color: ${palette.background-color};
      }

      #battery, #clock, #memory, #network, #pulseaudio, #temperature {
         padding-left: 7px;
         padding-right: 7px;
      }

      #system {
        background-color: ${palette.background-color};
        border: 1px solid ${palette.background_border-frame};
        border-radius: 5px;
        margin-left: 8px;
        padding: 0px 3px;
      }

      #battery {
        color: ${palette.orange};
      }
      #battery.warning {
        color: ${palette.yellow};
      }
      #battery.critical {
        color: ${palette.red};
      }
      #memory {
        color: ${palette.cyan};
      }
      #network {
        color: ${palette.green};
      }
      #network.disconnected {
        color: ${palette.red};
      }
      #pulseaudio {
        color: ${palette.magenta};
      }
      #pulseaudio.muted, #pulseaudio format-muted {
        color: ${palette.grey};
      }
      #temperature {
        color: ${palette.yellow};
      }

      #tray {
         padding-right: 28px;
         padding-left: 10px;
      }
    '';
  };
}
