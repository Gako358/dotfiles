{ osConfig
, config
, pkgs
, lib
, ...
}:
{
  config = lib.mkIf osConfig.environment.gaming.enable {
    home = {
      packages = with pkgs; [
        wowup-cf
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".config/WowUpCf"
        ];
      };
    };
  };
}
