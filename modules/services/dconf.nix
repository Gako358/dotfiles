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
            just-perfection.extensionUuid
            open-bar.extensionUuid
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
        "/org/gnome/shell/extensions/openbar" = {
          bartype = "Islands";
          trigger-reload = false;
          bgalpha = 0.0;
          reloadstyle = true;
          height = 48.0;
          margin = 7.3;
          set-overview = true;
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
          gnomeExtensions.just-perfection
          gnomeExtensions.open-bar
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
