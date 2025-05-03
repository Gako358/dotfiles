{ osConfig
, pkgs
, lib
, ...
}: {
  config = lib.mkMerge [
    (lib.mkIf osConfig.program.qemu.enable {
      dconf.settings = {
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = [ "qemu:///system" ];
          uris = [ "qemu:///system" ];
        };
      };
    })
    (lib.mkIf (osConfig.environment.desktop.windowManager == "gnome") {
      dconf.settings = {
        "org/gnome/TextEditor" = {
          keybindings = "vim";
        };
        "org/gnome/desktop/background" = {
          picture-uri = "file:///run/current-system/sw/share/backgrounds/gnome/symbolic-soup-l.jxl";
          picture-uri-dark = "file:///run/current-system/sw/share/backgrounds/gnome/symbolic-soup-d.jxl";
          primary-color = "#B9B5AE";
        };
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          enable-hot-corners = true;
        };
        "org/gnome/desktop/input-sources" = {
          sources = [ (lib.hm.gvariant.mkTuple [ "xkb" "us" ]) (lib.hm.gvariant.mkTuple [ "xkb" "no" ]) ];
        };
        "org/gnome/desktop/screensaver" = {
          picture-uri = "file:///run/current-system/sw/share/backgrounds/gnome/symbolic-soup-l.jxl";
          primary-color = "#B9B5AE";
          secondary-color = "#000000";
        };
        "org/gnome/desktop/session" = {
          idle-delay = lib.hm.gvariant.mkUint32 0;
        };
        "org/gnome/desktop/wm/keybindings" = {
          close = [ "<Alt>q" ];
          move-to-workspace-1 = [ "<Shift><Super>1" ];
          move-to-workspace-2 = [ "<Shift><Super>2" ];
          move-to-workspace-3 = [ "<Shift><Super>3" ];
          move-to-workspace-4 = [ "<Shift><Super>4" ];
          move-to-workspace-5 = [ "<Shift><Super>5" ];
          switch-to-workspace-1 = [ "<Super>1" ];
          switch-to-workspace-2 = [ "<Super>2" ];
          switch-to-workspace-3 = [ "<Super>3" ];
          switch-to-workspace-4 = [ "<Super>4" ];
          switch-to-workspace-5 = [ "<Super>5" ];
          toggle-fullscreen = [ "<Super>g" ];
        };
        "org/gnome/desktop/wm/preferences" = {
          focus-mode = "sloppy";
          mouse-button-modifier = "<Super>";
          num-workspaces = 5;
          resize-with-right-button = true;
          workspace-names = [ "Main" ];
        };
        "org/gnome/mutter" = {
          center-new-windows = true;
          dynamic-workspaces = false;
          edge-tiling = true;
          num-workspaces = 5;
          workspaces-only-on-primary = true;
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
          binding = "<Super>Return";
          command = "kgx";
          name = "console";
        };
        "org/gnome/shell" = {
          disable-user-extensions = false;
          enabled-extensions = [
            "space-bar@luchrioh"
            "trayIconsReloaded@selfmade.pl"
            "user-theme@gnome-shell-extensions.gcampax.github.com"
          ];
          favorite-apps = [
            "kgx.desktop"
            "discord.desktop"
            "evolution.desktop"
            "gimp.desktop"
            "spotify.desktop"
            "steam.desktop"
            "zen.desktop"
          ];
          last-selected-power-profile = "performance";
        };
        "org/gnome/shell/app-switcher" = {
          current-workspace-only = false;
        };
        "org/gnome/shell/extensions/user-theme" = {
          name = "palenight";
        };
        "org/gnome/shell/keybindings" = {
          switch-to-application-1 = [ ];
          switch-to-application-2 = [ ];
          switch-to-application-3 = [ ];
          switch-to-application-4 = [ ];
          switch-to-application-5 = [ ];
        };
        "system/locale" = {
          region = "nb_NO.UTF-8";
        };
      };

      home.packages = with pkgs; [
        gnomeExtensions.space-bar
        gnomeExtensions.sound-output-device-chooser
        gnomeExtensions.tray-icons-reloaded
        gnomeExtensions.user-themes
        palenight-theme
      ];
    })
  ];
}
