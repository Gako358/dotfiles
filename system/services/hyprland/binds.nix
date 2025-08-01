let
  mainMod = "SUPER";
  SECONDARY = "SHIFT";
  TERTIARY = "CTRL";

  toggle =
    program:
    let
      prog = builtins.substring 0 14 program;
    in
    "pkill ${prog} || uwsm app -- ${program}";

  runOnce = program: "pgrep ${program} || uwsm app -- ${program}";
in
{
  programs.hyprland.settings = {
    # Launchers
    bind = [
      "${mainMod}, Return, exec, ${toggle "alacritty"}"
      "${mainMod}, D, exec, ${toggle "wofi --show drun"}"
      "${mainMod}, B, exec, ${toggle "alacritty -t btop -e btm"}"
      "${mainMod}, R, exec, ${toggle "alacritty -t ranger -e ranger"}"
      "${mainMod}, S, exec, ${toggle "alacritty -t spotify_player -e spotify_player"}"
      "${mainMod} ${SECONDARY}, D, exec, ${runOnce "pcmanfm"}"

      # Lockscreen with screenshot
      "${mainMod} ${SECONDARY}, L, exec, ${runOnce "hyprlock"}"

      # Screenshot
      "${mainMod} ${SECONDARY}, P, exec, ${runOnce "grimshot --notify save area - | ${runOnce "swappy -f -"}"}"

      # Scratchpads
      "${mainMod} ${SECONDARY}, T, movetoworkspace, special"
      "${mainMod}, t, togglespecialworkspace"

      # Bindings
      "${mainMod} ${SECONDARY} ${TERTIARY}, Q, exit"
      "${mainMod}, Q, killactive"
      "${mainMod}, F, togglefloating"
      "${mainMod}, G, fullscreen"
      "${mainMod}, P, togglesplit"

      # Move focus with mainMod + arrow keys
      "${mainMod}, k, movefocus, u"
      "${mainMod}, j, movefocus, d"
      "${mainMod}, l, movefocus, r"
      "${mainMod}, h, movefocus, l"

      # Switch workspaces with mainMod + [0-9]
      "${mainMod}, left,  workspace, e-1"
      "${mainMod}, right, workspace, e+1"
      "${mainMod}, 1, workspace, 1"
      "${mainMod}, 2, workspace, 2"
      "${mainMod}, 3, workspace, 3"
      "${mainMod}, 4, workspace, 4"
      "${mainMod}, 5, workspace, 5"
      "${mainMod}, 6, workspace, 6"
      "${mainMod}, 7, workspace, 7"
      "${mainMod}, 8, workspace, 8"
      "${mainMod}, 9, workspace, 9"

      # Move active window to workspace
      "${mainMod} ${SECONDARY}, right, movetoworkspace, e+1"
      "${mainMod} ${SECONDARY}, left,  movetoworkspace, e-1"
      "${mainMod} ${SECONDARY}, 1, movetoworkspace, 1"
      "${mainMod} ${SECONDARY}, 2, movetoworkspace, 2"
      "${mainMod} ${SECONDARY}, 3, movetoworkspace, 3"
      "${mainMod} ${SECONDARY}, 4, movetoworkspace, 4"
      "${mainMod} ${SECONDARY}, 5, movetoworkspace, 5"
      "${mainMod} ${SECONDARY}, 6, movetoworkspace, 6"
      "${mainMod} ${SECONDARY}, 7, movetoworkspace, 7"
      "${mainMod} ${SECONDARY}, 8, movetoworkspace, 8"
      "${mainMod} ${SECONDARY}, 9, movetoworkspace, 9"
    ];

    # Window
    binde = [
      "${mainMod} ${TERTIARY}, k, resizeactive, 0 -20"
      "${mainMod} ${TERTIARY}, j, resizeactive, 0 20"
      "${mainMod} ${TERTIARY}, l, resizeactive, 20 0"
      "${mainMod} ${TERTIARY}, h, resizeactive, -20 0"
      "${mainMod} ALT,  k, moveactive, 0 -20"
      "${mainMod} ALT,  j, moveactive, 0 20"
      "${mainMod} ALT,  l, moveactive, 20 0"
      "${mainMod} ALT,  h, moveactive, -20 0"
    ];

    # Move/resize windows with mainMod + LMB/RMB and dragging
    bindm = [
      "${mainMod}, mouse:272, movewindow"
      "${mainMod}, mouse:273, resizewindow"
    ];
  };
}
