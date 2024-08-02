{pkgs, ...}: let
  eww = "${pkgs.eww}/bin/eww";
  wallpaper = "${pkgs.hyprpaper}/bin/hyprpaper";
in {
  wayland.windowManager.hyprland.settings = {
    env = [
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
      "HYPRCURSOR_THEME,${pkgs.capitaine-cursors}"
      "HYPRCURSOR_SIZE,16"
    ];

    exec-once = [
      "hyprctl setcursor capitaine-cursors-white 16}"
      "${wallpaper}"
      "${eww} daemon"
      "[workspace 1 silent] firefox"
      "[workspace 2 silent] alacritty"
      "[workspace 4 silent] teams-for-linux"
      "[workspace 7 silent] thunderbird"
    ];

    general = {
      gaps_in = 7;
      gaps_out = 7;
      border_size = 1;
      allow_tearing = true;
      resize_on_border = true;
    };
    decoration = {
      rounding = 16;
      blur = {
        enabled = true;
        brightness = 1.0;
        contrast = 1.0;
        noise = 0.01;

        vibrancy = 0.2;
        vibrancy_darkness = 0.5;

        passes = 4;
        size = 7;

        popups = true;
        popups_ignorealpha = 0.2;
      };

      drop_shadow = true;
      shadow_ignore_window = true;
      shadow_offset = "0 15";
      shadow_range = 100;
      shadow_render_power = 2;
      shadow_scale = 0.97;
      "col.shadow" = "rgba(00000055)";
    };
    animations = {
      enabled = true;
      animation = [
        "border, 1, 2, default"
        "fade, 1, 4, default"
        "windows, 1, 3, default, popin 80%"
        "workspaces, 1, 2, default, slide"
      ];
    };
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
      no_direct_scanout = false;
    };

    gestures = {
      workspace_swipe = true;
      workspace_swipe_forever = true;
    };

    xwayland.force_zero_scaling = true;
    debug.disable_logs = false;
  };
}
