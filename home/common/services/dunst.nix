{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.dunst;
in {
  options = {
    services.dunst = {
      enableModule = mkOption {
        type = types.bool;
        default = true;
        description = "Enable dunst module";
      };
    };
  };
  config = mkIf (cfg.enableModule && (config.desktop.environment == "dwm" || config.desktop.environment == "bspwm")) {
    services.dunst = {
      enable = true;
      settings = {
        global = {
          alignment = "center"; # Put message in the middle of the box
          browser = "xdg-open"; # use default browser to open links
          follow = "keyboard"; # follow keyboard focus
          font = "Monospace 11"; # Simple looking font
          frame_width = 3; # small frame
          geometry = "300x50-15+49";
          markup = "full"; # subset of HTML
          padding = 6; # distance between text and bubble border
          progress_bar = true; # show a progress bar in notification bubbles
          separator_color = "frame"; # use frame color to separate bubbles
          sort = true; # sort messages by urgency
        };
        urgency_low = {
          background = "#16161D";
          foreground = "#DCD7BA";
          frame_color = "#2D4F67";
          highlight = "#7E9CD8";
          timeout = 10;
        };
        urgency_normal = {
          background = "#16161D";
          foreground = "#DCD7BA";
          frame_color = "#2D4F67";
          highlight = "#7E9CD8";
          timeout = 10;
        };
        urgency_critical = {
          background = "#16161D";
          foreground = "#C34043";
          frame_color = "#E82424";
          highlight = "#FF9E3B";
          timeout = 0;
        };
        fullscreen_delay_everything = {
          # delay notifications by default
          fullscreen = "delay";
        };
        fullscreen_show_critical = {
          # show critical notification
          fullscreen = "show";
          msg_urgency = "critical";
        };
      };
    };
  };
}
