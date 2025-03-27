{ specialArgs, ... }: {
  wayland.windowManager.hyprland.extraConfig =
    if specialArgs.hidpi
    then ''
      workspace = 1, monitor:DP-2
      workspace = 2, monitor:DP-3
      workspace = 3, monitor:DP-3
      workspace = 4, monitor:DP-3
      workspace = 5, monitor:DP-2
      workspace = 6, monitor:HDMI-A-1
      workspace = 7, monitor:HDMI-A-1
      workspace = 8, monitor:HDMI-A-1
      workspace = 9, monitor:HDMI-A-1

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

      windowrule = workspace 1, class:^(firefox)$
      windowrule = workspace 2, class:^(Emacs)$
      windowrule = workspace 5, class:^(Wfica)$
      windowrule = workspace 6, class:^(steam)$
      windowrule = workspace 6, class:^(chromium-browser)$
      windowrule = workspace 7, class:^(thunderbird)$
      windowrule = workspace 8, class:^(Slack)$
      windowrule = workspace 9, title:^(discord.com_/channels/@me)$
      windowrule = workspace 9, title:^(Friends List)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(Spotify)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$

      windowrule = opacity 0.91 override 0.73 override, class:^(Emacs)$
    ''
    else ''
      workspace = 1, monitor:DP-8
      workspace = 2, monitor:DP-8
      workspace = 3, monitor:DP-8
      workspace = 4, monitor:DP-8
      workspace = 5, monitor:DP-8
      workspace = 6, monitor:eDP-1
      workspace = 7, monitor:eDP-1
      workspace = 8, monitor:eDP-1
      workspace = 9, monitor:eDP-1

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

      windowrule = workspace 1, class:^(firefox)$
      windowrule = workspace 2, class:^(Emacs)$
      windowrule = workspace 5, class:^(Wfica)$
      windowrule = workspace 6, class:^(steam)$
      windowrule = workspace 6, class:^(chromium-browser)$
      windowrule = workspace 7, class:^(thunderbird)$
      windowrule = workspace 8, class:^(Slack)$
      windowrule = workspace 9, title:^(discord.com_/channels/@me)$
      windowrule = workspace 9, title:^(Friends List)$

      windowrule = float, title:^(Spotify Premium)$
      windowrule = float, title:^(Spotify)$
      windowrule = float, title:^(ranger)$
      windowrule = float, title:^(btop)$

      windowrule = opacity 0.91 override 0.73 override, class:^(Emacs)$
    '';
}
