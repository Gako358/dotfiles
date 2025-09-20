{ config
, pkgs
, lib
, ...
}:
{
  config = lib.mkIf config.environment.gaming.enable {
    programs = {
      steam = {
        enable = true;
        extraCompatPackages = [
          pkgs.proton-ge-bin
        ];
        gamescopeSession.enable = true;
      };
    };
    environment.persistence."/persist" = {
      users.leif = {
        directories = [
          ".steam"
          ".local/share/Steam"
        ];
      };
    };
  };
}
