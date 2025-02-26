{pkgs, ...}: let
  fontSize = "14px";
  iconSize = "17px";
  opacity = "0.46";
  palette = {
    font = "RobotoMono Nerd Font";
    fontsize = fontSize;
    iconsize = iconSize;
    background-color = "rgba(39, 39, 39, ${opacity})";
    background = "#272727";
    foreground = "#aeafb0";
    dark = "#434343";
    red = "#c94f6d";
    yellow = "#dbc074";
    blue = "#719cd6";
    grey = "#665c54";
  };
  calendar = "${pkgs.gnome-calendar}/bin/gnome-calendar";
  system = "${pkgs.gnome-system-monitor}/bin/gnome-system-monitor";
in {
  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oa: {
      mesonFlags = (oa.mesonFlags or []) ++ ["-Dexperimental=true"];
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
        "temperature"
        "cpu"
        "battery"
        "memory"
        "pulseaudio"
        "network"
      ];
      clock = {
        format = " {:%a, %d %b, %I:%M %p}";
        tooltip = "true";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        format-alt = " {:%d/%m}";
        on-click = "${calendar}";
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

      "custom/launcher" = {
        format = "   ";
        tooltip = false;
      };
      battery = {
        states = {
          good = 95;
          warning = 30;
          critical = 15;
        };
        format = "{icon}  {capacity}%";
        format-charging = "  {capacity}%";
        format-plugged = " {capacity}% ";
        format-alt = "{icon} {time}";
        format-icons = ["" "" "" "" ""];
      };
      memory = {
        format = "󰍛 {}%";
        format-alt = "󰍛 {used}/{total} GiB";
        on-click = "${system}";
        interval = 5;
      };
      cpu = {
        format = "󰻠 {usage}%";
        format-alt = "󰻠 {avg_frequency} GHz";
        on-click = "${system}";
        interval = 5;
      };
      network = {
        format-wifi = "  {signalStrength}%";
        format-ethernet = "󰈀 100% ";
        tooltip-format = "Connected to {essid} {ifname} via {gwaddr}";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "󰖪 0% ";
      };
      tray = {
        icon-size = 20;
        spacing = 8;
      };
      pulseaudio = {
        format = "{icon} {volume}%";
        format-muted = "󰝟";
        format-icons = {
          default = ["󰕿" "󰖀" "󰕾"];
        };
        scroll-step = 5;
        on-click = "pavucontrol";
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

      #mode, #clock, #memory, #temperature,#cpu, #temperature, #pulseaudio, #network, #battery {
         padding-left: 7px;
         padding-right: 7px;
      }

      #tray {
         padding-right: 28px;
         padding-left: 10px;
      }
    '';
  };
}
