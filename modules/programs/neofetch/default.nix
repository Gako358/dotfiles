{ pkgs
, ...
}:
{
  home.packages = [ pkgs.neofetch ];
  xdg.configFile."neofetch/config.conf".source = ./neofetch.conf;
}
