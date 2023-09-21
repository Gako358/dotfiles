{pkgs, ...}: let
  mainMod = "SUPER";
  PRIMARY = "ALT";
  SECONDARY = "SHIFT";
  TERTIARY = "CTRL";

  idle = "${pkgs.swayidle}/bin/swayidle";
  grimshot = "${pkgs.sway-contrib.grimshot}/bin/grimshot";
  lock = "${pkgs.swaylock}/bin/swaylock";
  swappy = "${pkgs.swappy}/bin/swappy";
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
        size = 10
        passes = 3
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

    # Window rules
    windowrule = float, ^(Rofi)$
    windowrule = float, ^(Gimp-2.10)$
    windowrule = float, ^(org.gnome.Calculator)$
    windowrule = float, ^(org.gnome.Calendar)$
    windowrule = float, ^(pavucontrol)$
    windowrule = float, ^(nm-connection-editor)$
    windowrule = float, ^(Color Picker)$
    windowrule = float, ^(Network)$
    windowrule = float, ^(xdg-desktop-portal)$
    windowrule = float, ^(xdg-desktop-portal-gnome)$
    windowrule = float, ^(transmission-gtk)$
    windowrule = float, ^(chrome-teams.live.com__-Default)$

    windowrule = workspace 1, ^(firefox-beta)$
    windowrule = workspace 3, ^(Microsoft-edge)$
    windowrule = workspace 5, ^(Wfica)$
    windowrule = workspace 7, ^(thunderbird)$
    windowrule = workspace 9, ^(chrome-discord.com__channels_@me-Default)$



    windowrule = float, title:^(ranger)$
    windowrule = float, title:^(spotify)$
    windowrule = float, title:^(btop)$

    # Launchers
    bind = ${mainMod}, Return, exec, alacritty
    bind = ${mainMod}, D, exec, rofi -show drun -show-icon
    bind = ${mainMod}, R, exec, alacritty -t ranger -e ranger
    bind = ${mainMod}, N, exec, alacritty -t spotify -e ncspot
    bind = ${mainMod}, B, exec, alacritty -t btop -e btm

    # Lockscreen
    bind = ${mainMod} ${SECONDARY}, L, exec, swaylock -f -c 000000

    # Screenshot
    bind = ${mainMod} ${SECONDARY}, P, exec, ${grimshot} --notify save area - | ${swappy} -f -

    # Scratchpads
    bind = ${mainMod} ${SECONDARY}, T, movetoworkspace, special
    bind = ${mainMod}, grave, togglespecialworkspace

    # Bindings
    bind = ${mainMod} ${SECONDARY}, Q, exit
    bind = ${mainMod}, Q, killactive
    bind = ${mainMod}, F, togglefloating
    bind = ${mainMod}, G, fullscreen
    bind = ${mainMod}, O, fakefullscreen
    bind = ${mainMod}, P, togglesplit

    # Move/resize windows with mainMod + LMB/RMB and dragging
    bindm = ${mainMod}, mouse:272, movewindow
    bindm = ${mainMod}, mouse:273, resizewindow

    # Move focus with mainMod + arrow keys
    bind = ${mainMod}, k, movefocus, u
    bind = ${mainMod}, j, movefocus, d
    bind = ${mainMod}, l, movefocus, r
    bind = ${mainMod}, h, movefocus, l

    # Switch workspaces with mainMod + [0-9]
    bind = ${mainMod}, left,  workspace, e-1
    bind = ${mainMod}, right, workspace, e+1
    bind = ${mainMod}, 1, workspace, 1
    bind = ${mainMod}, 2, workspace, 2
    bind = ${mainMod}, 3, workspace, 3
    bind = ${mainMod}, 4, workspace, 4
    bind = ${mainMod}, 5, workspace, 5
    bind = ${mainMod}, 6, workspace, 6
    bind = ${mainMod}, 7, workspace, 7
    bind = ${mainMod}, 8, workspace, 8
    bind = ${mainMod}, 9, workspace, 9

    # Window
    binde = ${mainMod} ${TERTIARY}, k, resizeactive, 0 -20
    binde = ${mainMod} ${TERTIARY}, j, resizeactive, 0 20
    binde = ${mainMod} ${TERTIARY}, l, resizeactive, 20 0
    binde = ${mainMod} ${TERTIARY}, h, resizeactive, -20 0
    binde = ${mainMod} ALT,  k, moveactive, 0 -20
    binde = ${mainMod} ALT,  j, moveactive, 0 20
    binde = ${mainMod} ALT,  l, moveactive, 20 0
    binde = ${mainMod} ALT,  h, moveactive, -20 0

    # Move active window to workspace
    bind = ${mainMod} ${SECONDARY}, right, movetoworkspace, e+1
    bind = ${mainMod} ${SECONDARY}, left,  movetoworkspace, e-1
    bind = ${mainMod} ${SECONDARY}, 1, movetoworkspace, 1
    bind = ${mainMod} ${SECONDARY}, 2, movetoworkspace, 2
    bind = ${mainMod} ${SECONDARY}, 3, movetoworkspace, 3
    bind = ${mainMod} ${SECONDARY}, 4, movetoworkspace, 4
    bind = ${mainMod} ${SECONDARY}, 5, movetoworkspace, 5
    bind = ${mainMod} ${SECONDARY}, 6, movetoworkspace, 6
    bind = ${mainMod} ${SECONDARY}, 7, movetoworkspace, 7
    bind = ${mainMod} ${SECONDARY}, 8, movetoworkspace, 8
    bind = ${mainMod} ${SECONDARY}, 9, movetoworkspace, 9
  '';
}
