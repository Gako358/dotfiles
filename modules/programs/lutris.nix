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
        (lutris.override {
          extraPkgs = pkgs: [
            wineWowPackages.stable
            winetricks
          ];
        })
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".local/share/lutris"
        ];
      };
    };
  };
}
