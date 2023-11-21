{
  wayland.windowManager.hyprland.extraConfig = ''
    # Window rules
    windowrule = float, ^(Rofi)$
    windowrule = float, ^(eww)$
    windowrule = float, ^(Gimp-2.10)$
    windowrule = float, ^(org.gnome.Calculator)$
    windowrule = float, ^(org.gnome.Calendar)$
    windowrule = float, ^(gnome-system-monitor)$
    windowrule = float, ^(pavucontrol)$
    windowrule = float, ^(nm-connection-editor)$
    windowrule = float, ^(Color Picker)$
    windowrule = float, ^(Network)$
    windowrule = float, ^(xdg-desktop-portal)$
    windowrule = float, ^(xdg-desktop-portal-gnome)$
    windowrule = float, ^(transmission-gtk)$
    windowrule = float, ^(chrome-teams.live.com__-Default)$
    windowrule = float, ^(org.kde.kdeconnect-settings)$

    windowrule = workspace 1, ^(firefox-beta)$
    windowrule = workspace 3, ^(Microsoft-edge)$
    windowrule = workspace 7, ^(thunderbird)$
    windowrule = workspace 7, title:^(app.slack.com_/client/T04MZPW21RA/C04MUBWKREZ)$
    windowrule = workspace 5, ^(Wfica)$
    windowrule = workspace 9, title:^(discord.com_/channels/@me)$

    windowrule = float, title:^(ranger)$
    windowrule = float, title:^(spotify)$
    windowrule = float, title:^(btop)$
  '';
}
