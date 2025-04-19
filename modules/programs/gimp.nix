{ config
, pkgs
, lib
, ...
}:
{
  home.packages = lib.mkIf config.environment.desktop.enable [
    pkgs.gimp
  ];
}
