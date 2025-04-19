{ config
, pkgs
, lib
, ...
}:
{
  home = lib.mkIf config.desktop.environment.enable {
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
