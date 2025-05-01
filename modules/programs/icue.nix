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
        ckb-next
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          # TODO: Add icue needed folders
        ];
      };
    };
  };
}
