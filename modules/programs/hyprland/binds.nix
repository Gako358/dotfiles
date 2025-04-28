{ pkgs, ... }:
let
  mainMod = "SUPER";
  SECONDARY = "SHIFT";
  TERTIARY = "CTRL";

  # Dependencies
  grimshot = "${pkgs.sway-contrib.grimshot}/bin/grimshot";
  swappy = "${pkgs.swappy}/bin/swappy";
  wofi = "${pkgs.wofi}/bin/wofi";
  pcmanfm = "${pkgs.pcmanfm}/bin/pcmanfm";
  terminal = "${pkgs.alacritty}/bin/alacritty";
  lockScreen = "${pkgs.hyprlock}/bin/hyprlock";
in
{
  wayland.windowManager.hyprland.extraConfig = ''
    # Launchers
    bind = ${mainMod}, Return, exec, ${terminal}
    bind = ${mainMod}, D, exec, ${wofi} --show drun
    bind = ${mainMod}, B, exec, ${terminal} -t btop -e btm
    bind = ${mainMod}, R, exec, ${terminal} -t ranger -e ranger
    bind = ${mainMod}, S, exec, ${terminal} -t spotify_player -e spotify_player
    bind = ${mainMod} ${SECONDARY}, D, exec, ${pcmanfm}

    # Lockscreen with screenshot
    bind = ${mainMod} ${SECONDARY}, L, exec, ${lockScreen}

    # Screenshot
    bind = ${mainMod} ${SECONDARY}, P, exec, ${grimshot} --notify save area - | ${swappy} -f -

    # Scratchpads
    bind = ${mainMod} ${SECONDARY}, T, movetoworkspace, special
    bind = ${mainMod}, t, togglespecialworkspace

    # Bindings
    bind = ${mainMod} ${SECONDARY} ${TERTIARY}, Q, exit
    bind = ${mainMod}, Q, killactive
    bind = ${mainMod}, F, togglefloating
    bind = ${mainMod}, G, fullscreen
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
