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
        wineWow64Packages.wayland
        winetricks
        protontricks
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          # TODO: Add winetrix needed folders
        ];
      };
    };
  };
}
