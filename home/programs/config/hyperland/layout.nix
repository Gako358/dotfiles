{specialArgs, ...}: {
  wayland.windowManager.hyprland.extraConfig =
    if specialArgs.hidpi
    then ''
      input {
        kb_layout = us,no
        kb_options = grp:alt_shift_toggle
        follow_mouse = 1
        touchpad {
          natural_scroll = yes
        }
        sensitivity = 0
      }
    ''
    else ''
      input {
        kb_layout = us,no
        kb_options = grp:alt_shift_toggle
        kb_variant= colemak_dh_iso,,
        follow_mouse = 1
        touchpad {
          natural_scroll = yes
        }
        sensitivity = 0
      }
    '';
}
