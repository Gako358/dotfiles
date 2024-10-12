{
  lib,
  pkgs,
  specialArgs,
  ...
}: let
  wallpaper = "/home/merrinx/Sources/archive/images/wallpapers/dark_moon.jpg";
in
  with lib.hm.gvariant;
    if specialArgs.hidpi
    then {
      home.packages = with pkgs; [
        gnomeExtensions.auto-move-windows
        gnomeExtensions.blur-my-shell
        gnomeExtensions.caffeine
        gnomeExtensions.clipboard-history
        gnomeExtensions.dash-to-dock
        gnomeExtensions.just-perfection
        gnomeExtensions.pop-shell
        gnomeExtensions.rounded-window-corners
        gnomeExtensions.sound-output-device-chooser
        gnomeExtensions.space-bar
        gnomeExtensions.tray-icons-reloaded
        gnomeExtensions.user-themes
        gnome-tweaks
        dconf-editor
        flat-remix-gnome
        ranger
      ];
      dconf.settings = {
        "org/gnome/desktop/search-providers" = {
          disabled = ["org.gnome.Boxes.desktop"];
          enabled = ["org.gnome.Weather.desktop"];
          sort-order = [
            "org.gnome.Contacts.desktop"
            "org.gnome.Documents.desktop"
            "org.gnome.Nautilus.desktop"
            "org.gnome.Calendar.desktop"
            "org.gnome.Calculator.desktop"
            "org.gnome.Settings.desktop"
            "org.gnome.clocks.desktop"
            "org.gnome.design.IconLibrary.desktop"
            "org.gnome.seahorse.Application.desktop"
            "org.gnome.Weather.desktop"
            "org.gnome.Boxes.desktop"
          ];
        };

        "org/gnome/desktop/wm/keybindings" = {
          close = ["<Alt>q"];
          minimize = ["<Super>d"];
          move-to-workspace-1 = ["<Shift><Super>1"];
          move-to-workspace-2 = ["<Shift><Super>2"];
          move-to-workspace-3 = ["<Shift><Super>3"];
          move-to-workspace-4 = ["<Shift><Super>4"];
          move-to-workspace-5 = ["<Shift><Super>5"];
          switch-to-workspace-1 = ["<Super>1"];
          switch-to-workspace-2 = ["<Super>2"];
          switch-to-workspace-3 = ["<Super>3"];
          switch-to-workspace-4 = ["<Super>4"];
          switch-to-workspace-5 = ["<Super>5"];
          toggle-fullscreen = ["<Super>g"];
        };

        "org/gnome/shell/keybindings" = {
          switch-to-application-1 = [];
          switch-to-application-2 = [];
          switch-to-application-3 = [];
          switch-to-application-4 = [];
          switch-to-application-5 = [];
        };

        "org/gnome/desktop/wm/preferences" = {
          mouse-button-modifier = "<Super>";
          num-workspaces = 5;
          resize-with-right-button = true;
          focus-mode = "sloppy";
        };

        "org/gnome/mutter" = {
          dynamic-workspaces = false;
          edge-tiling = true;
          num-workspaces = 5;
          workspaces-only-on-primary = true;
        };

        "org/gnome/settings-daemon/plugins/media-keys" = {
          custom-keybindings = ["/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"];
          mic-mute = ["AudioMicMute"];
          next = ["AudioNext"];
          play = ["AudioPlay"];
          previous = ["AudioPrev"];
          stop = ["AudioStop"];
          volume-down = ["AudioLowerVolume"];
          volume-up = ["AudioRaiseVolume"];

          home = ["<Super>e"];
          www = ["<Super>w"];
          screensaver = ["<Super>l"];
        };

        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
          binding = "<Super>Return";
          command = "alacritty";
          name = "Terminal";
        };

        "org/gnome/settings-daemon/plugins/power" = {
          idle-dim = false;
          power-button-action = "interactive";
          sleep-inactive-ac-type = "nothing";
          sleep-inactive-battery-type = "nothing";
        };

        "org/gnome/desktop/background" = {
          "color-shading-type" = "solid";
          "picture-options" = "zoom";
          "picture-uri" = "file://${wallpaper}";
          "picture-uri-dark" = "file://${wallpaper}";
          "primary-color" = "#000000000000";
          "secondary-color" = "#000000000000";
        };

        "org/gnome/shell" = {
          enabled-extensions = [
            "auto-move-windows@gnome-shell-extensions.gcampax.github.com"
            "blur-my-shell@aunetx"
            "caffeine@patapon.info"
            "clipboard-history@alexsaveau.dev"
            "dash-to-dock@micxgx.gmail.com"
            "just-perfection-desktop@just-perfection"
            "pop-shell@system76.com"
            "rounded-window-corners@yilozt"
            "sound-output-device-chooser@kgshank.net"
            "space-bar@luchrioh"
            "trayIconsReloaded@selfmade.pl"
            "user-theme@gnome-shell-extensions.gcampax.github.com"
          ];

          favorite-apps = [
            "firefox.desktop"
            "thunderbird.desktop"
            "org.gnome.Nautilus.desktop"
            "org.gnome.Calendar.desktop"
            "spotify.desktop"
            "Alacritty.desktop"
            "Chromium.desktop"
          ];
        };

        "org/gnome/shell/extensions/auto-move-windows" = {
          "application-list" = [
            "firefox.desktop:1"
            "Alacritty.desktop:2"
          ];
        };

        "org/gnome/shell/extensions/blur-my-shell" = {
          "settings-version" = 2;
        };

        "org/gnome/shell/extensions/blur-my-shell/dash-to-dock" = {
          "pipeline" = "pipeline_default_rounded";
        };

        "org/gnome/shell/extensions/blur-my-shell/lockscreen" = {
          "pipeline" = "pipeline_default";
        };

        "org/gnome/shell/extensions/blur-my-shell/overview" = {
          "pipeline" = "pipeline_default";
        };

        "org/gnome/shell/extensions/blur-my-shell/panel" = {
          "pipeline" = "pipeline_default";
        };

        "org/gnome/shell/extensions/blur-my-shell/screenshot" = {
          "pipeline" = "pipeline_default";
        };

        "org/gnome/shell/extensions/clipboard-history" = {
          "display-mode" = 3;
          "next-entry" = ["<Shift><Alt>j"];
          "prev-entry" = ["<Shift><Alt>k"];
          "toggle-menu" = ["<Shift><Alt>v"];
        };

        "org/gnome/shell/extensions/dash-to-dock" = {
          "animate-show-apps" = false;
          "apply-custom-theme" = false;
          "autohide" = true;
          "background-color" = "rgb(24,25,38)";
          "background-opacity" = 0.8;
          "custom-background-color" = true;
          "custom-theme-shrink" = true;
          "dash-max-icon-size" = 37;
          "dock-fixed" = false;
          "dock-position" = "BOTTOM";
          "extend-height" = false;
          "height-fraction" = 0.9;
          "hot-keys" = false;
          "intellihide" = false;
          "intellihide-mode" = "FOCUS_APPLICATION_WINDOWS";
          "preferred-monitor" = -2;
          "preferred-monitor-by-connector" = "DisplayPort-0";
          "preview-size-scale" = 0.0;
          "show-show-apps-button" = false;
          "show-trash" = false;
          "transparency-mode" = "DYNAMIC";
        };

        "org/gnome/shell/extensions/just-perfection" = {
          "accessibility-menu" = true;
          "activities-button" = true;
          "activities-button-icon-monochrome" = false;
          "activities-button-label" = true;
          "animation" = 0;
          "app-menu" = false;
          "app-menu-icon" = true;
          "app-menu-label" = true;
          "background-menu" = true;
          "clock-menu" = true;
          "controls-manager-spacing-size" = 0;
          "dash" = true;
          "dash-icon-size" = 0;
          "dash-separator" = true;
          "double-super-to-appgrid" = true;
          "gesture" = true;
          "hot-corner" = false;
          "keyboard-layout" = true;
          "notification-banner-position" = 2;
          "osd" = false;
          "panel" = true;
          "panel-arrow" = true;
          "panel-button-padding-size" = 4;
          "panel-corner-size" = 0;
          "panel-in-overview" = true;
          "panel-indicator-padding-size" = 0;
          "panel-notification-icon" = true;
          "panel-size" = 0;
          "power-icon" = true;
          "quick-settings" = true;
          "ripple-box" = true;
          "screen-sharing-indicator" = true;
          "search" = true;
          "show-apps-button" = true;
          "startup-status" = 0;
          "theme" = false;
          "window-demands-attention-focus" = false;
          "window-menu-take-screenshot-button" = true;
          "window-picker-icon" = true;
          "window-preview-caption" = true;
          "window-preview-close-button" = true;
          "workspace" = false;
          "workspace-background-corner-size" = 0;
          "workspace-peek" = false;
          "workspace-popup" = true;
          "workspace-switcher-size" = 0;
          "workspace-wrap-around" = false;
          "workspaces-in-app-grid" = true;
        };

        "org/gnome/shell/extensions/pop-shell" = {
          "active-hint" = false;
          "gap-inner" = mkUint32 5;
          "gap-outer" = mkUint32 5;
          "tile-by-default" = true;
        };

        "org/gnome/shell/extensions/rounded-window-corners" = {
          "border-color" = mkTuple [0.71764707565307617 0.74117660522460938 0.97254902124404907 1.0];
          "border-width" = 1;
          global-rounded-corner-settings = [
            (mkDictionaryEntry [
              "padding"
              (mkVariant [
                (mkDictionaryEntry ["top" (mkVariant 1)])
                (mkDictionaryEntry ["left" (mkVariant 1)])
                (mkDictionaryEntry ["right" (mkVariant 1)])
                (mkDictionaryEntry ["bottom" (mkVariant 1)])
              ])
            ])

            (mkDictionaryEntry [
              "keep_rounded_corners"
              (mkVariant [
                (mkDictionaryEntry ["maximized" (mkVariant true)])
                (mkDictionaryEntry ["fullscreen" (mkVariant false)])
              ])
            ])

            (mkDictionaryEntry ["border_radius" (mkVariant 8)])
            (mkDictionaryEntry ["smoothing" (mkVariant 0)])
            (mkDictionaryEntry ["enabled" (mkVariant true)])
          ];
          "skip-libadwaita-app" = false;
          "skip-libhandy-app" = false;
        };

        "org/gnome/shell/extensions/space-bar/appearance" = {
          "inactive-workspace-text-color" = "rgb(154,153,150)";
          "workspace-margin" = 3;
          "workspaces-bar-padding" = 3;
        };

        "org/gnome/shell/extensions/space-bar/behavior" = {
          "scroll-wheel" = "panel";
          "show-empty-workspaces" = false;
          "smart-workspace-names" = false;
          "toggle-overview" = false;
        };

        "org/gnome/shell/extensions/space-bar/shortcuts" = {
          "enable-activate-workspace-shortcuts" = true;
          "enable-move-to-workspace-shortcuts" = true;
        };

        "org/gnome/shell/app-switcher" = {
          current-workspace-only = false;
        };

        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };

        "org/gnome/TextEditor" = {
          keybindings = "vim";
        };
      };
    }
    else {}
