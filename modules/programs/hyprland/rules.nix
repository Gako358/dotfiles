{
  wayland.windowManager.hyprland.extraConfig = ''
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
    windowrule = workspace 5, class:^(.virt-manager-wrapped)$
    windowrule = workspace 7, class:^(steam)$
    windowrule = workspace 7, title:^(Friends List)$
    windowrule = workspace 8, class:^(Slack)$
    windowrule = workspace 9, class:^(discord)$

    windowrule = float, title:^(Spotify Premium)$
    windowrule = float, title:^(Spotify)$
    windowrule = float, title:^(spotify_player)$
    windowrule = float, title:^(ranger)$
    windowrule = float, title:^(btop)$

    windowrule = opacity 0.91 override 0.73 override, class:^(Emacs)$
  '';
}
