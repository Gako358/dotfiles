{ config
, pkgs
, lib
, ...
}:
{
  config = lib.mkIf config.environment.desktop.enable {
    home = {
      packages = [
        pkgs.discord
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".config/discord"
        ];
      };
    };

    xdg.configFile = {
      "discord/settings.json" = {
        text = ''
          {
            "SKIP_HOST_UPDATE": true
          }
        '';
      };
    };
  };
}
