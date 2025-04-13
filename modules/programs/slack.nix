{ pkgs, ... }:
{
  home = {
    packages = [
      pkgs.slack
    ];
    persistence."/persist/home/merrinx" = {
      directories = [
        ".config/Slack"
      ];
    };
  };
}
