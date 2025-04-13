{ pkgs, config, ... }:
{
  home = {
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
