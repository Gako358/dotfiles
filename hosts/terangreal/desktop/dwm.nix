{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm") {
    # If running dwm
    services.xserver = {
      windowManager.dwm.enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          greeters.enso.enable = true;
          background = nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
        };
        defaultSession = "none+dwm";
      };
    };
  };
}
