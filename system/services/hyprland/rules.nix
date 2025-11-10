{
  programs.hyprland.settings = {
    windowrule = [
      "float, class:^(Rofi)$"
      "float, class:^(eww)$"
      "float, class:^(Gimp-2.10)$"
      "float, class:^(org.gnome.Calculator)$"
      "float, class:^(org.gnome.Calendar)$"
      "float, class:^(gnome-system-monitor)$"
      "float, class:^(pavucontrol)$"
      "float, class:^(nm-connection-editor)$"
      "float, class:^(Color Picker)$"
      "float, class:^(Network)$"
      "float, class:^(pcmanfm)$"
      "float, class:^(com.github.flxzt.rnote)$"
      "float, class:^(xdg-desktop-portal)$"
      "float, class:^(xdg-desktop-portal-gnome)$"
      "float, class:^(transmission-gtk)$"
      "float, class:^(org.kde.kdeconnect-settings)$"
      "float, class:^(org.pulseaudio.pavucontrol)$"

      "workspace 1, class:^(zen)$"
      "workspace 2, class:^(Emacs)$"
      "workspace 3, class:^(Alacritty)$"
      "workspace 5, class:^(Wfica)$"
      "workspace 5, class:^(.virt-manager-wrapped)$"
      "workspace 7, class:^(steam)$"
      "workspace 7, title:^(Friends List)$"
      "workspace 8, class:^(Slack)$"
      "workspace 9, class:^(discord)$"

      "float, title:^(Spotify Premium)$"
      "float, title:^(Spotify)$"
      "float, title:^(spotify_player)$"
      "float, title:^(ranger)$"
      "float, title:^(btop)$"

      "opacity 0.91 override 0.73 override, class:^(Emacs)$"
    ];
  };
}
