{ pkgs, config, ... }:
{
  home = {
    packages = [
      pkgs.spotifywm
    ];
    persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/spotify"
      ];
    };
  };
}
