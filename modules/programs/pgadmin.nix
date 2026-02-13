{ osConfig
, pkgs
, lib
, ...
}:
let
  inherit (osConfig.environment) desktop;
in
{
  config = lib.mkIf (desktop.enable && desktop.develop) {
    home.packages = [
      pkgs.pgadmin4-desktopmode
    ];
  };
}
