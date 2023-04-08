{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; let
  color = {
    background = "#32302f";
    foreground = "#ebdbb2";
    foreground-alt = "#FBF1C7";
  };
  cfg = config.desktop;
in
{
  config = mkIf (cfg.environment == "bspwm") {
    services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        iwSupport = true;
        githubSupport = true;
        pulseSupport = true;
      };
      script = "exec polybar bar-search & polybar bar-left & polybar bar-center & polybar bar-right & polybar bar-power &";
      config = {
        "settings" = { screenchange-reload = true; };
        "color" = {
          background = "#32302F";
          foreground = "#EBDBB2";
          foreground-alt = "#FBF1C7";
        };
        "global/wm" = {
          margin-top = "0";
          margin-bottom = "0";
        };
        "module/alsa" = {
          type = "internal/alsa";

          master-soundcard = "default";
          speaker-soundcard = "default";
          headphone-soundcard = "default";
          master-mixer = "Master";
          interval = "5";

          format-volume = "<ramp-volume> <label-volume>";
          format-volume-background = "${color.background}";
          format-volume-padding = "2";

          format-muted = "<label-muted>";
          format-muted-prefix = "婢";
          format-muted-background = "${color.background}";
          format-muted-padding = "2";
          label-volume = "%percentage%%";
          label-muted = " ";
          label-muted-foreground = "${color.foreground}";

          ramp-volume-0 = "奄";
          ramp-volume-1 = "奔";
          ramp-volume-2 = "墳";
          ramp-headphones-0 = "";
        };
        "module/battery" = {
          type = "internal/battery";

          full-at = "98";
          labal-full = "Full";
          battery = "BAT1";
          interval = "5";

          format-charging = "<ramp-charging> <label-charging>";
          format-charging-background = "${color.background}";
          format-charging-padding = "2";

          format-discharging = "<ramp-discharging> <label-discharging>";
          format-discharging-background = "${color.background}";
          format-discharging-padding = "2";

          label-charging = "%percentage%%";
          label-charging-foreground = "${color.foreground}";

          label-discharging = "%percentage%%";
          label-discharging-foreground = "${color.foreground}";

          ramp-charging-0 = "";
          ramp-charging-1 = "";
          ramp-charging-2 = "";
          ramp-charging-3 = "";
          ramp-charging-4 = "";
          ramp-charging-5 = "";
          ramp-charging-6 = "";
          ramp-charging-7 = "";
          ramp-charging-8 = "";
          ramp-charging-9 = "";

          ramp-discharging-0 = "";
          ramp-discharging-1 = "";
          ramp-discharging-2 = "";
          ramp-discharging-3 = "";
          ramp-discharging-4 = "";
          ramp-discharging-5 = "";
          ramp-discharging-6 = "";
          ramp-discharging-7 = "";
          ramp-discharging-8 = "";
          ramp-discharging-9 = "";

          ramp-charging-framerate = "750";
        };
        "module/cpu" = {
          type = "internal/cpu";

          interval = "5";
          format = "<label>";
          format-prefix = "﬙";
          format-background = "${color.background}";
          format-padding = "2";

          label = "%percentage%%";
        };
        "module/date" = {
          type = "internal/date";
          interval = "1.0";
          time = " %I:%M %p";
          time-alt = " %a, %d %b %Y";
          format = "<label>";
          format-background = "${color.background}";
          format-padding = "2";
          label = "%time%";
        };
        "module/filesystem" = {
          type = "internal/fs";

          mount-0 = "/";
          interval = "60";
          fixed-values = "true";

          format-mounted = "<label-mounted>";
          format-mounted-prefix = "";
          format-mounted-background = "${color.background}";
          format-mounted-padding = "2";

          format-unmounted = "<label-unmounted>";
          format-unmounted-prefix = "";
          format-unmounted-background = "${color.background}";
          format-unmounted-padding = "2";

          label-mounted = " %free%";
          label-unmounted = " %mountpoint%: not mounted";
        };
        "module/memory" = {
          type = "internal/memory";

          interval = "5";
          format = "<label>";
          format-prefix = "";
          format-background = "${color.background}";
          format-padding = "2";

          label = "%mb_used%";
        };
        "module/network" = {
          type = "internal/network";

          interface = "wlp5s0";
          interval = "5";
          format-connected = "<label-connected>";
          format-connected-background = "${color.background}";
          format-connected-padding = "2";
          format-connected-prefix = "直";
          format-connected-prefix-foreground = "${color.foreground}";

          label-connected = " %essid% %ip%";
          label-connected-foreground = "${color.foreground}";
        };
        "module/title" = {
          type = "internal/xwindow";

          interval = "1.0";
          format = "<label>";
          format-prefix = "﯑";
          format-background = "${color.background}";
          format-padding = "2";

          label = "%title%";
          label-maxlen = "30";
        };
        "module/workspaces" = {
          type = "internal/bspwm";

          pin-workspaces = "true";
          enable-click = "true";
          enable-scroll = "true";

          icon-0 = "1;";
          icon-1 = "2;";
          icon-2 = "3;";
          icon-3 = "4;";
          icon-4 = "5;";
          icon-default = "";

          format = "<label-state>";
          format-padding = "1";
          format-background = "${color.background}";

          label-monitor = "%name%";
          label-active = "";
          label-active-foreground = "${color.foreground}";
          label-occupied = "";
          label-occupied-foreground = "${color.foreground}";
          label-urgent = "";
          label-urgent-foreground = "${color.background}";

          label-empty = "";

          label-active-padding = "1";
          label-urgent-padding = "1";
          label-occupied-padding = "1";
          label-empty-padding = "1";
        };
        "module/launcher" = {
          type = "custom/text";
          content = "";
          content-background = "${color.background}";
          content-foreground = "${color.foreground}";
          content-padding = "3";
          click-left = "";
        };
        "module/sysmenu" = {
          type = "custom/text";
          content = "襤";

          content-background = "${color.background}";
          content-foreground = "${color.foreground}";
          content-padding = "2";

          click-left = "";
        };
        # Bars
        "bar/bar-search" = {
          monitor = "";
          monitor-fallback = "";
          monitor-strict = "false";
          override-redirect = "false";
          bottom = "false";
          fixed-center = "true";

          width = "4%";
          height = "30";
          offset-x = "1%";
          offset-y = "1.5%";

          background = "${color.background}";
          foreground = "${color.foreground}";

          underline-size = "2";
          underline-color = "${color.foreground}";

          border-size = "0";
          border-color = "${color.foreground}";

          font-0 = "JetBrains Mono Nerd Font:pixelsize=13;3";
          font-1 = "Iosevka Nerd Font:pixelsize=15;4";

          modules-center = "launcher";

          separator = "|";
          dim-value = "1.0";

          wm-restack = "bspwm";
        };
        "bar/bar-left" = {
          monitor = "";
          monitor-fallback = "";
          monitor-strict = "false";
          override-redirect = "false";
          bottom = "false";
          fixed-center = "true";

          width = "21%";
          height = "30";
          offset-x = "6%";
          offset-y = "1.5%";

          background = "${color.background}";
          foreground = "${color.foreground}";

          underline-size = "2";
          underline-color = "${color.foreground}";

          border-size = "0";
          border-color = "${color.foreground}";

          font-0 = "JetBrains Mono Nerd Font:pixelsize=13;3";
          font-1 = "Iosevka Nerd Font:pixelsize=15;4";

          modules-center = "cpu filesystem memory";

          separator = "|";
          dim-value = "1.0";

          wm-restack = "bspwm";
        };
        "bar/bar-center" = {
          monitor = "";
          monitor-fallback = "";
          monitor-strict = "false";
          override-redirect = "false";
          bottom = "false";
          fixed-center = "true";

          width = "20%";
          height = "30";
          offset-x = "40%";
          offset-y = "1.5%";

          background = "${color.background}";
          foreground = "${color.foreground}";

          underline-size = "2";
          underline-color = "${color.foreground}";

          border-size = "0";
          border-color = "${color.foreground}";

          font-0 = "JetBrains Mono Nerd Font:pixelsize=13;3";
          font-1 = "Iosevka Nerd Font:pixelsize=15;4";

          modules-center = "workspaces";

          separator = "|";
          dim-value = "1.0";

          wm-restack = "bspwm";
        };
        "bar/bar-right" = {
          monitor = "";
          monitor-fallback = "";
          monitor-strict = "false";
          override-redirect = "false";
          bottom = "false";
          fixed-center = "true";

          width = "22%";
          height = "30";
          offset-x = "72%";
          offset-y = "1.5%";

          background = "${color.background}";
          foreground = "${color.foreground}";

          underline-size = "2";
          underline-color = "${color.foreground}";

          border-size = "0";
          border-color = "${color.foreground}";

          font-0 = "JetBrains Mono Nerd Font:pixelsize=13;3";
          font-1 = "Iosevka Nerd Font:pixelsize=15;4";

          modules-center = "alsa battery network date";

          separator = "|";
          dim-value = "1.0";

          wm-restack = "bspwm";
        };
        "bar/bar-power" = {
          monitor = "";
          monitor-fallback = "";
          monitor-strict = "false";
          override-redirect = "false";
          bottom = "false";
          fixed-center = "true";

          width = "4%";
          height = "30";
          offset-x = "95%";
          offset-y = "1.5%";

          background = "${color.background}";
          foreground = "${color.foreground}";

          underline-size = "2";
          underline-color = "${color.foreground}";

          border-size = "0";
          border-color = "${color.foreground}";

          font-0 = "JetBrains Mono Nerd Font:pixelsize=15;3";
          font-1 = "Iosevka Nerd Font:pixelsize=17;4";

          modules-center = "sysmenu";

          separator = "|";
          dim-value = "1.0";

          wm-restack = "bspwm";
        };
        "SETTINGS" = {
          throttle-output = "5";
          throttle-output-for = "10";
          screenchange-reload = "false";
          compositing-background = "source";
          compositing-foreground = "over";
          compositing-overline = "over";
          compositing-underline = "over";
          compositing-border = "over";
          pseudo-transparency = "false";
        };
      };
    };
  };
}
