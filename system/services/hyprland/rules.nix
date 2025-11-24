{
  programs.hyprland.settings = {
    windowrule = [
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Rofi)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(eww)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Gimp-2.10)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.gnome.Calculator)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.gnome.Calendar)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(gnome-system-monitor)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(pavucontrol)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(nm-connection-editor)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Color Picker)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Network)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(pcmanfm)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(com.github.flxzt.rnote)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(xdg-desktop-portal)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(xdg-desktop-portal-gnome)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(transmission-gtk)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.kde.kdeconnect-settings)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.pulseaudio.pavucontrol)$"

      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(Spotify Premium)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(Spotify)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(spotify_player)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(ranger)$"
      "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(btop)$"

      "opacity 0.91 override 0.73 override, match:class ^(Emacs)$"

      "workspace 1, match:class ^(zen)$"
      "workspace 2, match:class ^(Emacs)$"
      "workspace 3, match:class ^(Alacritty)$"
      "workspace 5, match:class ^(Wfica)$"
      "workspace 5, match:class ^(.virt-manager-wrapped)$"
      "workspace 7, match:class ^(steam)$"
      "workspace 7, match:title ^(Friends List)$"
      "workspace 8, match:class ^(Slack)$"
      "workspace 9, match:class ^(discord)$"
    ];
  };
}
