{
  osConfig,
  pkgs,
  lib,
  ...
}:
let
  warcraftlogs = pkgs.callPackage ../../pkgs/warcraftlogs { };
in
{
  config = lib.mkIf osConfig.environment.gaming.enable {
    home = {
      packages = [ warcraftlogs ];
      persistence."/persist/" = {
        directories = [
          ".config/Archon App Lite"
        ];
      };
    };
  };
}
