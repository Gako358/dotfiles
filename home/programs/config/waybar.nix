{pkgs, ...}: let
  opacity = "0";
  fontSize = "16px";
  iconSize = "22px";
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
  calendar = "${pkgs.gnome.gnome-calendar}/bin/gnome-calendar";
  system = "${pkgs.gnome.gnome-system-monitor}/bin/gnome-system-monitor";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
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
      margin-top = 0;
      margin-bottom = 0;
      margin-left = 0;
      margin-right = 0;
      modules-left = [
        "custom/launcher"
        "custom/playerctl#backward"
        "custom/playerctl#play"
        "custom/playerctl#foward"
        "custom/playerlabel"
      ];
      modules-center = [
        "hyprland/workspaces"
      ];
      modules-right = [
        "tray"
        "cpu"
        "battery"
        "memory"
        "pulseaudio"
        "network"
        "clock"
      ];
      clock = {
        format = " {:%a, %d %b, %I:%M %p}";
        tooltip = "true";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        format-alt = " {:%d/%m}";
        on-click = "${calendar}";
      };
      "custom/layout" = {
        format = " {}";
        exec = ''
          hyprctl devices -j |
          jq -r '.keyboards[] | .active_keymap' |
          head -n1 |
          cut -c1-2 |
          tr 'a-z' 'A-Z'
        '';
        interval = 1;
      };
      "custom/playerctl#backward" = {
        format = "󰙣 ";
        on-click = "${playerctl} previous";
        on-scroll-up = "${playerctl} volume .05+";
        on-scroll-down = "${playerctl} volume .05-";
      };
      "custom/playerctl#play" = {
        format = "{icon}";
        return-type = "json";
        exec = "${playerctl} -a metadata --format '{\"text\": \"{{artist}} - {{markup_escape(title)}}\", \"tooltip\": \"{{playerName}} : {{markup_escape(title)}}\", \"alt\": \"{{status}}\", \"class\": \"{{status}}\"}' -F";
        on-click = "${playerctl} play-pause";
        on-scroll-up = "${playerctl} volume .05+";
        on-scroll-down = "${playerctl} volume .05-";
        format-icons = {
          Playing = "<span>󰏥 </span>";
          Paused = "<span> </span>";
          Stopped = "<span> </span>";
        };
      };
      "custom/playerctl#foward" = {
        format = "󰙡 ";
        on-click = "${playerctl} next";
        on-scroll-up = "${playerctl} volume .05+";
        on-scroll-down = "${playerctl} volume .05-";
      };
      "custom/playerlabel" = {
        format = "<span>󰎈 {} 󰎈</span>";
        return-type = "json";
        max-length = 40;
        exec = "${playerctl} -a metadata --format '{\"text\": \"{{artist}} - {{markup_escape(title)}}\", \"tooltip\": \"{{playerName}} : {{markup_escape(title)}}\", \"alt\": \"{{status}}\", \"class\": \"{{status}}\"}' -F";
        on-click = "";
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
      "custom/launcher" = {
        format = "";
        tooltip = "false";
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
          border-radius: 16px;
          color: #${palette.primary_accent}
      }
      #workspaces button {
          padding: 0px 5px;
          margin: 0px 3px;
          border-radius: 16px;
          background: ${palette.primary_background_rgba};
          transition: all 0.3s ease-in-out;
      }

      #workspaces button.active {
          background-color: #${palette.secondary_accent};
          color: #${palette.background};
          border-radius: 16px;
          min-width: 50px;
          background-size: 400% 400%;
          transition: all 0.3s ease-in-out;
      }

      #workspaces button:hover {
          background-color: #${palette.tertiary_accent};
          color: #${palette.background};
          border-radius: 16px;
          min-width: 50px;
          background-size: 400% 400%;
      }

      #tray, #pulseaudio, #network, #battery, #cpu, #memory,
      #custom-playerctl.backward, #custom-playerctl.play, #custom-playerctl.foward, #custom-layout{
          background: #${palette.tertiary_background_hex};
          font-weight: bold;
          margin: 5px 0px;
      }
      #tray, #pulseaudio, #network, #battery, #cpu, #memory, #custom-layout{
          color: #${palette.tertiary_accent};
          border-radius: 10px 24px 10px 24px;
          padding: 0 20px;
          margin-left: 7px;
      }
      #clock {
          color: #${palette.tertiary_accent};
          background: #${palette.tertiary_background_hex};
          border-radius: 0px 0px 0px 40px;
          padding: 10px 10px 15px 25px;
          margin-left: 7px;
          font-weight: bold;
          font-size: ${palette.fontsize};
      }
      #custom-launcher {
          color: #${palette.secondary_accent};
          background: #${palette.tertiary_background_hex};
          border-radius: 0px 0px 40px 0px;
          margin: 0px;
          padding: 0px 35px 0px 15px;
          font-size: 28px;
      }

      #custom-playerctl.backward, #custom-playerctl.play, #custom-playerctl.foward {
          background: #${palette.tertiary_background_hex};
          font-size: ${palette.iconsize};
      }
      #custom-playerctl.backward:hover, #custom-playerctl.play:hover, #custom-playerctl.foward:hover{
          color: #${palette.tertiary_accent};
      }
      #custom-playerctl.backward {
          color: #${palette.primary_accent};
          border-radius: 24px 0px 0px 10px;
          padding-left: 16px;
          margin-left: 7px;
      }
      #custom-playerctl.play {
          color: #${palette.secondary_accent};
          padding: 0 5px;
      }
      #custom-playerctl.foward {
          color: #${palette.primary_accent};
          border-radius: 0px 10px 24px 0px;
          padding-right: 12px;
          margin-right: 7px
      }
      #custom-playerlabel, #custom-currentplayer{
          background: #${palette.tertiary_background_hex};
          color: #${palette.tertiary_accent};
          padding: 0 20px;
          border-radius: 24px 10px 24px 10px;
          margin: 5px 0;
          font-weight: bold;
      }
      #window{
          background: #${palette.tertiary_background_hex};
          padding-left: 15px;
          padding-right: 15px;
          border-radius: 16px;
          margin-top: 5px;
          margin-bottom: 5px;
          font-weight: normal;
          font-style: normal;
      }
    '';
  };
}
