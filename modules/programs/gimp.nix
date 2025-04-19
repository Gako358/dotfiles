{ config
, pkgs
, lib
, ...
}:
{
  home.packages = lib.mkIf config.desktop.environment.enable [
    pkgs.gimp
  ];
}
