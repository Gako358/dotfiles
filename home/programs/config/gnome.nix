{pkgs, ...}: {
  home.packages = with pkgs; [
    gnome.gnome-tweaks
    gnomeExtensions.user-themes
    gnomeExtensions.tray-icons-reloaded
    gnomeExtensions.vitals
    gnomeExtensions.dash-to-panel
    gnomeExtensions.space-bar
    gnomeExtensions.tiling-assistant
    gnome.dconf-editor
    gnomeExtensions.caffeine
    flat-remix-gnome
  ];
  dconf.settings = {
    "org/gnome/tweaks".show-extensions-notice = false;
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "caffeine@patapon.info"
        "dash-to-panel@jderose9.github.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "sound-output-device-chooser@kgshank.net"
        "space-bar@luchrioh"
        "tiling-assistant@leleat-on-github"
        "trayIconsReloaded@selfmade.pl"
        "Vitals@CoreCoding.com"
      ];
      favorite-apps = [
        "firefox.desktop"
        "thunderbird.desktop"
        "org.gnome.Nautilus.desktop"
        "dbeaver.desktop"
        "spotify.desktop"
        "code.desktop"
        "gimp.desktop"
      ];
    };
    # Keybindings
    "org/gnome/settings-daemon/plugins/media-keys" = {
      email = ["<Super>e"];
      www = ["<Super>w"];
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
      ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<super>return";
      command = "alacritty";
      name = "open-terminal";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      binding = "<shift><super>r";
      command = "alacritty -e ranger";
      name = "Ranger";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      binding = "<shift><super>b";
      command = "alacritty -e btm";
      name = "Btop";
    };

    # Windows
    "org.gnome.desktop.wm.keybindings" = {
      switch-to-workspace-1 = ["<Super>1"];
      switch-to-workspace-2 = ["<Super>2"];
      switch-to-workspace-3 = ["<Super>3"];
      switch-to-workspace-4 = ["<Super>4"];
      move-to-workspace-1 = ["<Super><Shift>1"];
      move-to-workspace-2 = ["<Super><Shift>2"];
      move-to-workspace-3 = ["<Super><Shift>3"];
      move-to-workspace-4 = ["<Super><Shift>4"];
      close = ["<Super>q"];
      close-other-windows = ["<Super><Shift>q"];
      close-window = ["<Super>q"];
    };
    # Custom
    "org/gnome/shell/extensions/user-theme".name = "Flat-Remix-Blue-Light";
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
    "org/gnome/desktop/wm/preferences" = {
      workspace-names = ["Main"];
    };
  };
}
