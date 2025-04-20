{ osConfig
, config
, pkgs
, lib
, ...
}:
{
  home = lib.mkIf osConfig.environment.desktop.enable {
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
