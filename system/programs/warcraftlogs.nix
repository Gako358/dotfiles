{
  config,
  pkgs,
  lib,
  ...
}:
let
  warcraftlogs = pkgs.callPackage ../../pkgs/warcraftlogs { };
in
{
  config = lib.mkIf config.environment.gaming.enable {
    environment.systemPackages = [ warcraftlogs ];
  };
}
