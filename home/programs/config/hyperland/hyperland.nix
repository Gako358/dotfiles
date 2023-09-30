{pkgs, ...}: let
  # Dependencies
  eww = "${pkgs.eww-wayland}/bin/eww";
  idle = "${pkgs.swayidle}/bin/swayidle";
  lock = "${pkgs.swaylock}/bin/swaylock";
  wallpaper = "${pkgs.hyprpaper}/bin/hyprpaper";
in {
  wayland.windowManager.hyprland.extraConfig = ''
    general {
      layout = dwindle
      resize_on_border = true
    }

    misc {
      layers_hog_keyboard_focus = false
      layers_hog_keyboard_focus = true
      disable_splash_rendering = true
      disable_hyprland_logo = true
    }

    binds {
      allow_workspace_cycles = true
    }

    dwindle {
      pseudotile = yes
      preserve_split = yes
    }

    master {
      new_is_master = false
    }

    gestures {
      workspace_swipe = on
    }

    exec-once = ${wallpaper}
    exec-once= ${eww} daemon

    # Auto lock
    exec ${idle} -w \
      timeout 120 '${lock} -f' \
      timeout 180 'swaymsg "output * dpms off"' \
        resume 'swaymsg "output * dpms on"' \
      before-sleep '${lock} -f'

    decoration {
      drop_shadow = yes
      shadow_range = 8
      shadow_render_power = 2
      col.shadow = rgba(00000044)
      dim_inactive = false

      blur {
        enabled = true
        size = 8
        passes = 4
        new_optimizations = on
        noise = 0.01
        contrast = 0.9
        brightness = 0.8
      }
    }

    animations {
      enabled = yes
      bezier = myBezier, 0.05, 0.9, 0.1, 1.05
      animation = windows, 1, 5, myBezier
      animation = windowsOut, 1, 7, default, popin 80%
      animation = border, 1, 10, default
      animation = fade, 1, 7, default
      animation = workspaces, 1, 6, default
    }
  '';
}
