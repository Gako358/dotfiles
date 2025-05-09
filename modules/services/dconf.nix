{ osConfig
, pkgs
, lib
, ...
}:
let
  autostartPrograms = [ pkgs.discord pkgs.ckb-next ];
in
{
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

      gtk.cursorTheme.size = lib.mkForce 24;

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
          enabled-extensions = with pkgs.gnomeExtensions; [
            caffeine.extensionUuid
            clipboard-indicator.extensionUuid
            dash-to-panel.extensionUuid
            just-perfection.extensionUuid
            space-bar.extensionUuid
            tray-icons-reloaded.extensionUuid
            user-themes.extensionUuid
          ];
          favorite-apps = [
            "org.gnome.Console.desktop"
            "discord.desktop"
            "gimp.desktop"
            "org.gnome.Nautilus.desktop"
            "spotify.desktop"
            "steam.desktop"
            "zen.desktop"
          ];
        };
        "org/gnome/shell/app-switcher" = {
          current-workspace-only = false;
        };
        "org/gnome/shell/extensions/caffeine" = {
          enable-fullscreen = true;
          restore-state = true;
          show-indicator = true;
          show-notification = false;
          user-enabled = true;
        };
        "org/gnome/shell/extensions/dash-to-panel" = {
          window-preview-fixed-x = true;
          window-preview-fixed-y = true;
          preview-custom-opacity = 73;
          window-preview-size = 127;
          appicon-padding = 5;
          appicon-margin = 3;
          show-tooltip = false;
          show-showdesktop-hover = true;
          dot-style-unfocused = "DOTS";
          dot-style-focused = "DOTS";
          trans-use-custom-opacity = true;
          trans-panel-opacity = "0.55";
          tray-size = 19;
          leftbox-size = 21;
          panel-element-positions = ''{"AUS-0x0000bec6":[{"element":"showAppsButton","visible":true,"position":"stackedTL"},{"element":"activitiesButton","visible":false,"position":"stackedTL"},{"element":"leftBox","visible":true,"position":"stackedTL"},{"element":"taskbar","visible":true,"position":"centered"},{"element":"centerBox","visible":true,"position":"stackedBR"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"dateMenu","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":true,"position":"stackedBR"}]}'';
          panel-sizes = "{\"0\":64}";
          panel-positions = "{\"0\":\"BOTTOM\"}";
          showdesktop-button-width = "5";
          show-apps-icon-file = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake-white.svg";
        };
        "org/gnome/shell/extensions/just-perfection" = {
          accessibility-menu = true;
          app-menu = true;
          app-menu-icon = true;
          background-menu = true;
          clock-menu = false;
          controls-manager-spacing-size = 22;
          dash = true;
          dash-icon-size = 0;
          double-super-to-appgrid = true;
          gesture = true;
          hot-corner = false;
          notification-banner-position = 2;
          osd = false;
          panel = true;
          panel-arrow = true;
          panel-corner-size = 1;
          panel-in-overview = true;
          panel-notification-icon = true;
          panel-size = 36;
          power-icon = true;
          ripple-box = false;
          search = false;
          show-apps-button = true;
          startup-status = 0;
          theme = true;
          window-demands-attention-focus = true;
          window-picker-icon = false;
          window-preview-caption = true;
          window-preview-close-button = true;
          workspace = true;
          workspace-background-corner-size = 15;
          workspace-popup = false;
          workspaces-in-app-grid = true;
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

      home = {
        file = builtins.listToAttrs (map
          (pkg:
            {
              name = ".config/autostart/" + pkg.pname + ".desktop";
              value =
                if pkg ? desktopItem then {
                  inherit (pkg.desktopItem) text;
                } else {
                  source = pkg + "/share/applications/" + pkg.pname + ".desktop";
                };
            })
          autostartPrograms);

        packages = with pkgs; [
          gnomeExtensions.caffeine
          gnomeExtensions.clipboard-indicator
          gnomeExtensions.dash-to-panel
          gnomeExtensions.just-perfection
          gnomeExtensions.space-bar
          gnomeExtensions.sound-output-device-chooser
          gnomeExtensions.tray-icons-reloaded
          gnomeExtensions.user-themes
          palenight-theme
        ];
      };
    })
  ];
}
