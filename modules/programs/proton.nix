{ osConfig
, pkgs
, lib
, ...
}:
{
  config = lib.mkIf osConfig.environment.desktop.enable {
    home = {
      packages = [
        pkgs.proton-pass
      ];
      persistence."/persist/" = {
        directories = [
          ".config/Proton\ Pass"
        ];
      };
    };
  };
}
