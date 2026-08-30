_: {
  flake.nixosModules.services-kde =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (config.environment) desktop;
      kdePersistenceDirectories = [
        {
          directory = ".config";
          mode = "0700";
        }
        {
          directory = ".local/share";
          mode = "0700";
        }
      ];
      persistenceUsers = desktop.kde.persistenceUsers;
    in
    {
      options.environment.desktop.kde = {
        persistenceDirectories = lib.mkOption {
          type = lib.types.listOf (
            lib.types.oneOf [
              lib.types.str
              (lib.types.submodule {
                options = {
                  directory = lib.mkOption { type = lib.types.str; };
                  mode = lib.mkOption { type = lib.types.str; };
                };
              })
            ]
          );
          default = kdePersistenceDirectories;
          readOnly = true;
        };
        persistenceUsers = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Users whose XDG config/data is persisted; selected users must not have nested persistence paths.";
        };
      };

      config = lib.mkIf (desktop.windowManager == "kde") {
        assertions = [
          {
            assertion = lib.all (
              user: lib.hasAttr user config.users.users && config.users.users.${user}.isNormalUser
            ) persistenceUsers;
            message = "environment.desktop.kde.persistenceUsers must contain existing normal users";
          }
        ];
        environment.persistence."/persist".users = lib.genAttrs persistenceUsers (_: {
          directories = desktop.kde.persistenceDirectories;
        });
        services = {
          displayManager.sddm = {
            enable = true;
            enableHidpi = true;
            settings.Theme.CursorTheme = "Yaru";
            wayland.enable = true;
          };
          desktopManager.plasma6.enable = true;
        };

        environment = {
          systemPackages = [
            pkgs.yaru-theme
            (pkgs.writeTextDir "share/sddm/themes/breeze/theme.conf.user" ''
              [General]
              background=${desktop.theme.wallpaper};
              type=image
            '')
          ];
          plasma6.excludePackages = with pkgs.kdePackages; [
            baloo-widgets
            elisa
            ffmpegthumbs
            kate
            khelpcenter
            konsole
            krdp
            plasma-browser-integration
          ];
        };
        # Disabled redundant services
        systemd.user.services = {
          "app-org.kde.discover.notifier@autostart".enable = false;
          "app-org.kde.kalendarac@autostart".enable = false;
        };
      };
    };
}
