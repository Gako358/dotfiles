{specialArgs, ...}: {
  wayland.windowManager.hyprland.extraConfig =
    if specialArgs.hidpi
    then ''
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
      windowrule = float, ^(org.kde.kdeconnect-settings)$

      windowrule = workspace 1, ^(firefox)$
      windowrule = workspace 3, ^(microsoft-edge)$
      windowrule = workspace 3, ^(chrome-teams.live.com__-Default)$
      windowrule = workspace 7, title:^(discord.com_/channels/@me)$
      windowrule = workspace 7, ^(thunderbird)$
      windowrule = workspace 7, title:^(app.slack.com_/client/T04MZPW21RA/C04MUBWKREZ)$
      windowrule = workspace 5, ^(Wfica)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$
    ''
    else ''
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
      windowrule = float, ^(pcmanfm)$
      windowrule = float, ^(xdg-desktop-portal)$
      windowrule = float, ^(xdg-desktop-portal-gnome)$
      windowrule = float, ^(transmission-gtk)$
      windowrule = float, ^(org.kde.kdeconnect-settings)$

      windowrule = workspace 1, ^(firefox)$
      windowrule = workspace 3, ^(microsoft-edge)$
      windowrule = workspace 3, ^(chrome-teams.live.com__-Default)$
      windowrule = workspace 5, ^(Wfica)$
      windowrule = workspace 7, ^(thunderbird)$
      windowrule = workspace 8, title:^(app.slack.com_/client/T04MZPW21RA/C04MUBWKREZ)$
      windowrule = workspace 9, title:^(discord.com_/channels/@me)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$
    '';
}
