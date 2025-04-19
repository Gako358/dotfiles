{ config
, pkgs
, lib
, ...
}:
{
  config = lib.mkIf config.desktop.environment.enable {
    home.packages = [ pkgs.neofetch ];
    xdg.configFile."neofetch/config.conf".source = ./neofetch.conf;
  };
}
