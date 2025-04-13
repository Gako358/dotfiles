{ specialArgs, ... }: {
  wayland.windowManager.hyprland.extraConfig =
    let
      workspaceConfig =
        if specialArgs.master then ''
          workspace = 1, monitor:DP-3
          workspace = 2, monitor:DP-2
          workspace = 3, monitor:DP-2
          workspace = 4, monitor:DP-2
          workspace = 5, monitor:DP-2
          workspace = 6, monitor:DP-3
          workspace = 7, monitor:DP-3
          workspace = 8, monitor:DP-3
          workspace = 9, monitor:DP-3
        '' else ''
          workspace = 1, monitor:DP-6
          workspace = 2, monitor:DP-8
          workspace = 3, monitor:DP-8
          workspace = 4, monitor:DP-8
          workspace = 5, monitor:DP-8
          workspace = 6, monitor:DP-6
          workspace = 7, monitor:DP-6
          workspace = 8, monitor:DP-6
          workspace = 9, monitor:DP-6
        '';

      windowRules = ''
        windowrule = float, class:^(Rofi)$
        windowrule = float, class:^(eww)$
        windowrule = float, class:^(Gimp-2.10)$
        windowrule = float, class:^(org.gnome.Calculator)$
        windowrule = float, class:^(org.gnome.Calendar)$
        windowrule = float, class:^(gnome-system-monitor)$
        windowrule = float, class:^(pavucontrol)$
        windowrule = float, class:^(nm-connection-editor)$
        windowrule = float, class:^(Color Picker)$
        windowrule = float, class:^(Network)$
        windowrule = float, class:^(pcmanfm)$
        windowrule = float, class:^(com.github.flxzt.rnote)$
        windowrule = float, class:^(xdg-desktop-portal)$
        windowrule = float, class:^(xdg-desktop-portal-gnome)$
        windowrule = float, class:^(transmission-gtk)$
        windowrule = float, class:^(org.kde.kdeconnect-settings)$
        windowrule = float, class:^(org.pulseaudio.pavucontrol)$

        windowrule = workspace 1, class:^(zen)$
        windowrule = workspace 5, class:^(Wfica)$
        windowrule = workspace 7, class:^(steam)$
        windowrule = workspace 7, title:^(Friends List)$
        windowrule = workspace 8, class:^(Slack)$
        windowrule = workspace 9, class:^(discord)$

        windowrule = float, title:^(Spotify Premium)$
        windowrule = float, title:^(Spotify)$
        windowrule = float, title:^(ranger)$
        windowrule = float, title:^(btop)$

        windowrule = opacity 0.91 override 0.73 override, class:^(Emacs)$
      '';
    in
    workspaceConfig + "\n" + windowRules;
}
