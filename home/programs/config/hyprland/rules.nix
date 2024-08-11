{specialArgs, ...}: {
  wayland.windowManager.hyprland.extraConfig =
    if specialArgs.hidpi
    then ''
      workspace = 2, monitor:DP-3
      workspace = 1, monitor:DP-2
      workspace = 4, monitor:HDMI-A-1
      workspace = 5, monitor:DP-2
      workspace = 7, monitor:HDMI-A-1
      workspace = 8, monitor:HDMI-A-1
      workspace = 9, monitor:HDMI-A-1

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
      windowrule = float, ^(com.github.flxzt.rnote)$
      windowrule = float, ^(xdg-desktop-portal)$
      windowrule = float, ^(xdg-desktop-portal-gnome)$
      windowrule = float, ^(transmission-gtk)$
      windowrule = float, ^(org.kde.kdeconnect-settings)$

      windowrule = workspace 1, ^(firefox)$
      windowrule = workspace 4, ^(chromium-browser)$
      windowrule = workspace 5, ^(Wfica)$
      windowrule = workspace 6, ^(steam)$
      windowrule = workspace 7, ^(thunderbird)$
      windowrule = workspace 8, title:^(app.slack.com_/client/T04MZPW21RA/C04MUBWKREZ)$
      windowrule = workspace 9, title:^(discord.com_/channels/@me)$
      windowrule = workspace 9, title:^(Friends List)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(Spotify)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$

      windowrule = opacity 0.73 override 0.64 override, ^(Alacritty)$
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
      windowrule = float, ^(com.github.flxzt.rnote)$
      windowrule = float, ^(xdg-desktop-portal)$
      windowrule = float, ^(xdg-desktop-portal-gnome)$
      windowrule = float, ^(transmission-gtk)$
      windowrule = float, ^(org.kde.kdeconnect-settings)$

      windowrule = workspace 1, ^(firefox)$
      windowrule = workspace 4, ^(chromium-browser)$
      windowrule = workspace 5, ^(Wfica)$
      windowrule = workspace 6, ^(steam)$
      windowrule = workspace 7, ^(thunderbird)$
      windowrule = workspace 8, title:^(app.slack.com_/client/T04MZPW21RA/C04MUBWKREZ)$
      windowrule = workspace 9, title:^(discord.com_/channels/@me)$
      windowrule = workspace 9, title:^(Friends List)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(Spotify)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$

      windowrule = opacity 0.73 override 0.64 override, ^(Alacritty)$
    '';
}
