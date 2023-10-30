{
  pkgs,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    gnomeExtensions.battery-indicator-upower
    gnomeExtensions.caffeine
    gnomeExtensions.clipboard-indicator
    gnomeExtensions.dash-to-panel
    gnomeExtensions.just-perfection
    gnomeExtensions.user-themes
    gnomeExtensions.tray-icons-reloaded
    gnomeExtensions.vitals
    gnomeExtensions.space-bar
    gnome.dconf-editor
    gnome.gnome-tweaks
    gnomeExtensions.pop-shell
    flat-remix-gnome
  ];
  dconf.settings = {
    "org/gnome/tweaks".show-extensions-notice = false;
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "caffeine@patapon.info"
        "clipboard-indicator@tudmotu.com"
        "dash-to-panel@jderose9.github.com"
        "just-perfection-desktop@just-perfection"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "sound-output-device-chooser@kgshank.net"
        "space-bar@luchrioh"
        "tiling-assistant@leleat-on-github"
        "trayIconsReloaded@selfmade.pl"
        "Vitals@CoreCoding.com"
      ];
      favorite-apps = [
        "firefox-beta.desktop"
        "thunderbird.desktop"
        "org.gnome.Nautilus.desktop"
        "spotify.desktop"
        "dbeaver.desktop"
        "code.desktop"
        "discord.desktop"
        "gimp.desktop"
        "microsoft-edge.desktop"
        "slack.desktop"
      ];
    };

    "org/gnome/shell/extensions/just-perfection" = {
      theme = true;
      activities-button = false;
      app-menu = true;
      animation = lib.hm.gvariant.mkUint32 3;
      clock-menu-position = lib.hm.gvariant.mkUint32 1;
      clock-menu-position-offset = lib.hm.gvariant.mkUint32 7;
    };

    "org/gnome/shell/extensions/caffeine" = {
      enable-fullscreen = true;
      restore-state = true;
      show-indicator = true;
      show-notification = false;
    };

    "org/gnome/shell/extension/dash-to-panel" = {
      # Possibly need to set this manually
      panel-position = ''{"0":"Bottom","1":"Bottom"}'';
      panel-sizes = ''{"0":55,"1":55}'';
      panel-element-positions-monitors-sync = true;
      appicon-margin = lib.hm.gvariant.mkUint32 0;
      appicon-padding = lib.hm.gvariant.mkUint32 3;
      dot-position = "TOP";
      dot-style-focused = "SOLID";
      dot-style-unfocused = "DOTS";
      animate-appicon-hover = true;
      animate-appicon-hover-animation-travel = "{'SIMPLE': 0.14999999999999999, 'RIPPLE': 0.40000000000000002, 'PLANK': 0.0}";
      isolate-monitors = true;
    };

    # Keybindings
    "org/gnome/settings-daemon/plugins/media-keys" = {
      email = ["<Super>e"];
      www = ["<Super>w"];
      screensaver = ["<Shift><Super>P"];
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
    "org/gnome/desktop/wm/keybindings" = {
      # Activate the window menu
      activate-window-menu = [];
      # Move window
      begin-move = [];
      # Resize window
      begin-resize = ["<Super>r"];
      # Close window
      close = ["<Super>q"];
      # Switch windows of an app directly
      cycle-group = [];
      cycle-group-backward = [];
      # Switch system controls directly
      cycle-panels = [];
      cycle-panels-backward = [];
      # Switch windows directly
      cycle-windows = [];
      cycle-windows-backward = [];
      # Maximize window
      maximize = ["<Super>Up"];
      # Minimize window
      minimize = ["<Super>c"];
      # Move window to workspace 1
      move-to-workspace-1 = ["<Shift><Super>exclam"];
      # Move window to workspace 2
      move-to-workspace-2 = ["<Shift><Super>at"];
      # Move window to workspace 3
      move-to-workspace-3 = ["<Shift><Super>numbersign"];
      # Move window to workspace 4
      move-to-workspace-4 = ["<Shift><Super>dollar"];
      # Switch to workspace 1
      switch-to-workspace-1 = ["<Super>1"];
      # Switch to workspace 2
      switch-to-workspace-2 = ["<Super>2"];
      # Switch to workspace 3
      switch-to-workspace-3 = ["<Super>3"];
      # Switch to workspace 4
      switch-to-workspace-4 = ["<Super>4"];
      # Switch windows
      switch-windows = ["<Super>Tab"];
      switch-windows-backward = ["<Shift><Super>Tab"];
      # Toggle fullscreen mode
      toggle-fullscreen = [];
      # Toggle maximization state
      toggle-maximized = ["<Super>f"];
      # Restore window
      unmaximize = ["<Super>Down"];
    };
    "org/gnome/shell/keybindings" = {
      # Focus the active notification
      focus-active-notification = [];
      # Open the application menu
      open-application-menu = [];
      # Switch to application 1
      switch-to-application-1 = [];
      # Switch to application 2
      switch-to-application-2 = [];
      # Switch to application 3
      switch-to-application-3 = [];
      # Switch to application 4
      switch-to-application-4 = [];
      # Switch to application 5
      switch-to-application-5 = [];
      # Switch to application 6
      switch-to-application-6 = [];
      # Switch to application 7
      switch-to-application-7 = [];
      # Switch to application 8
      switch-to-application-8 = [];
      # Switch to application 9
      switch-to-application-9 = [];
      # Show all applications
      toggle-application-view = [];
      # Show the notification list
      toggle-message-tray = ["<Super>n"];
      # Show the overview
      toggle-overview = [];
    };
    # Custom
    "org/gnome/shell/extensions/user-theme".name = "Flat-Remix-Blue-Light";
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
    "org/gnome/desktop/wm/preferences" = {
      workspace-names = ["Main"];
      "focus-mode" = lib.hm.gvariant.mkValue "mouse";
      "titlebar-font" = lib.hm.gvariant.mkString "Roboto Bold 11";
    };
  };
}
