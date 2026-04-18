{
  config,
  pkgs,
  lib,
  ...
}:
let
  raiderio-client = pkgs.callPackage ../../pkgs/raiderio-client { };
in
{
  config = lib.mkIf config.environment.gaming.enable {
    environment.systemPackages = [ raiderio-client ];
  };
}
