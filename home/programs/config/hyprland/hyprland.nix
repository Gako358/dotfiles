{pkgs, ...}: let
  wallpaper = "${pkgs.hyprpaper}/bin/hyprpaper";
in {
  wayland.windowManager.hyprland.settings = {
    exec-once = [
      "${wallpaper}"
      "hyprctl setcursor capitaine-cursors-white 16"
      "[workspace 1 silent] firefox"
      "[workspace 2 silent] alacritty"
      "[workspace 6 silent] chromium"
      "[workspace 7 silent] thunderbird"
      "wl-clip-persist --clipboard both &"
      "wl-paste --watch cliphist store &"
      "dbus-update-activation-environment --all --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
      "systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
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
      blur.enabled = true;
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
    };

    gestures = {
      workspace_swipe = true;
      workspace_swipe_forever = true;
    };

    xwayland.force_zero_scaling = true;
    debug.disable_logs = false;
  };
}
