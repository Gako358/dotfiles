{pkgs, ...}: let
  opacity = "0";
  fontSize = "14px";
  iconSize = "17px";
  palette = {
    font = "RobotoMono Nerd Font";
    fontsize = fontSize;
    iconsize = iconSize;
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
  # Dependencies
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
      height = 37;
      margin-top = 3;
      margin-bottom = 0;
      margin-left = 4;
      margin-right = 4;
      modules-left = [
        "hyprland/workspaces"
      ];
      modules-center = [
        "clock"
      ];
      modules-right = [
        "tray"
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
          background: ${palette.primary_background_rgba};
      }

      #workspaces {
          background: #${palette.tertiary_background_hex};
          margin: 5px 5px;
          padding: 8px 5px;
          border-radius: 15px;
          color: #${palette.primary_accent}
      }
      #workspaces button {
          padding: 0px 5px;
          margin: 0px 3px;
          border-radius: 15px;
          background: ${palette.primary_background_rgba};
          transition: all 0.3s ease-in-out;
      }

      #workspaces button.active {
          background-color: #${palette.secondary_accent};
          color: #${palette.background};
          border-radius: 15px;
          min-width: 50px;
          background-size: 400% 400%;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button:hover {
          background-color: #${palette.tertiary_accent};
          color: #${palette.background};
          border-radius: 15px;
          min-width: 50px;
          background-size: 400% 400%;
      }

      #tray, #pulseaudio, #network, #battery, #cpu, #memory{
          background: #${palette.tertiary_background_hex};
          font-weight: bold;
          margin: 5px 0px;
      }
      #tray, #pulseaudio, #network, #battery, #cpu, #memory{
          color: #${palette.tertiary_accent};
          border-radius: 15px;
          padding: 0 7px;
          margin-left: 7px;
      }
      #clock {
          color: #${palette.tertiary_accent};
          background: #${palette.tertiary_background_hex};
          border-radius: 15px;
          padding: 0px 10px;
          font-weight: bold;
          font-size: ${palette.fontsize};
      }
    '';
  };
}
