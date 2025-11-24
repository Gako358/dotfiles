{ pkgs, ... }:
{
  programs.hyprland.settings = {
    env = [
      "GRIMBLAST_NO_CURSOR,0"
      "HYPRCURSOR_THEME,${pkgs.capitaine-cursors}"
      "HYPRCURSOR_SIZE,16"
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
    ];
    exec-once = [
      "hyprpaper"
      "hyprctl setcursor capitaine-cursors-white 16"
      "wl-clip-persist --clipboard both &"
      "wl-paste --watch cliphist store &"
      "uwsm finalize"
      "[workspace 1 silent] zen"
      "[workspace 2 silent] emacsclient -c -n"
      "[workspace 8 silent] slack"
      "[workspace 9 silent] discord"
    ];

    general = {
      gaps_in = 7;
      gaps_out = 7;
      border_size = 2;
      "col.active_border" = "rgb(B48EAD) rgb(5E81AC) rgb(719cd6) 45deg";
      "col.inactive_border" = "rgb(3B4252)";
      hover_icon_on_border = true;
      extend_border_grab_area = 15;
      allow_tearing = true;
      resize_on_border = true;
    };
    cursor.inactive_timeout = 5;
    decoration = {
      rounding = 16;
      blur = {
        enabled = true;
        size = 4;
        passes = 2;
        new_optimizations = true;
        ignore_opacity = true;
        xray = false;
        contrast = 1.1;
        brightness = 1.0;
        noise = 0.02;
      };
      active_opacity = 1.0;
      inactive_opacity = 0.95;
      fullscreen_opacity = 1.0;
    };
    layerrule = [
      "blur on, match:namespace ^(wofi)$"
      "ignore_alpha 0, match:namespace ^(wofi)$"
      "blur on, match:namespace ^(waybar)$"
      "ignore_alpha 0, match:namespace ^(waybar)$"
      "blur on, match:namespace ^(notifications)$"
      "ignore_alpha 0, match:namespace ^(notifications)$"
    ];
    animations.enabled = true;
    animation = [
      "windows, 1, 6, wind, slide"
      "windowsIn, 1, 6, winIn, slide"
      "windowsOut, 1, 5, winOut, slide"
      "windowsMove, 1, 5, wind, slide"
      "border, 1, 10, liner"
      "borderangle, 1, 60, liner, loop"
      "fade, 1, 10, default"
      "workspaces, 1, 6, overshot, slidevert"
      "specialWorkspace, 1, 6, default, slidevert"
    ];
    bezier = [
      "wind, 0.05, 0.9, 0.1, 1.05"
      "winIn, 0.1, 1.1, 0.1, 1.1"
      "winOut, 0.3, -0.3, 0, 1"
      "liner, 1, 1, 1, 1"
      "overshot, 0.13, 0.99, 0.29, 1.1"
    ];
    group = {
      groupbar = {
        font_size = 10;
        gradients = false;
      };
    };
    input = {
      kb_layout = "us,no";
      kb_options = "grp:alt_shift_toggle";
    };
    dwindle = {
      pseudotile = true;
      preserve_split = true;
    };
    misc = {
      disable_autoreload = true;
      force_default_wallpaper = 0;
      animate_mouse_windowdragging = false;
      vrr = 1;
    };

    xwayland.force_zero_scaling = true;
    debug.disable_logs = false;
  };
}
