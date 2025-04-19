{ config
, pkgs
, lib
, ...
}:
{
  home = lib.mkIf config.environment.desktop.enable {
    packages = [
      pkgs.slack
    ];
    persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/Slack"
      ];
    };
  };
}
