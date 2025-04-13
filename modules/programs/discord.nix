{ pkgs, ... }:
{
  home = {
    packages = [
      pkgs.discord
    ];
    persistence."/persist/home/merrinx" = {
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
}
