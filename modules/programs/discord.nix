{ pkgs, ... }:
{
  home.packages = [
    pkgs.discord
  ];

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
