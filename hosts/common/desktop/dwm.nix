{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm") {
    environment.systemPackages = with pkgs; [
      # GUI Core
      thunderbird
      firefox
      geany
      gimp
    ];
    # If running dwm
    services.xserver = {
      windowManager.dwm.enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          greeters.enso.enable = true;
          background = pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
        };
        defaultSession = "none+dwm";
      };
    };
  };
}
