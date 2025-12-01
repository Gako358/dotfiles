{ osConfig
, config
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
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".config/Proton\ Pass"
        ];
      };
    };
  };
}
