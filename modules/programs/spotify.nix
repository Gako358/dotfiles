{ pkgs, ... }:
{
  home = {
    packages = [
      pkgs.spotifywm
    ];
    persistence."/persist/home/merrinx" = {
      directories = [
        ".config/spotify"
      ];
    };
  };
}
