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
        autoLoginUser = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "User automatically logged into KDE by SDDM.";
        };
        displayServer = lib.mkOption {
          type = lib.types.enum [
            "wayland"
            "x11"
          ];
          default = "wayland";
          description = "Display server used by KDE and SDDM.";
        };
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
            assertion =
              desktop.kde.autoLoginUser == null
              || (
                lib.hasAttr desktop.kde.autoLoginUser config.users.users
                && config.users.users.${desktop.kde.autoLoginUser}.isNormalUser
              );
            message = "environment.desktop.kde.autoLoginUser must name an existing normal user";
          }
          {
            assertion = lib.all (
              user: lib.hasAttr user config.users.users && config.users.users.${user}.isNormalUser
            ) persistenceUsers;
            message = "environment.desktop.kde.persistenceUsers must contain existing normal users";
          }
        ];
        environment.persistence."/persist".users = lib.genAttrs persistenceUsers (user: {
          directories = desktop.kde.persistenceDirectories;
          files = lib.optional (user == "farstrider") ".zen";
        });
        services = {
          xserver.enable = desktop.kde.displayServer == "x11";
          displayManager = {
            autoLogin = {
              enable = desktop.kde.autoLoginUser != null;
              user = desktop.kde.autoLoginUser;
            };
            sddm = {
              enable = true;
              enableHidpi = true;
              settings.Theme.CursorTheme = "Yaru";
              theme = "breeze";
              wayland.enable = desktop.kde.displayServer == "wayland";
            };
          };
          desktopManager.plasma6.enable = true;
        };

        environment = {
          systemPackages = [ pkgs.yaru-theme ];
          plasma6.excludePackages = with pkgs.kdePackages; [
            baloo-widgets
            elisa
            ffmpegthumbs
            kate
            khelpcenter
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
