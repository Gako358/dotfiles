{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop.kde;
in {
  options.desktop.kde.enable = lib.mkEnableOption "KDE Plasma Desktop";
  config = lib.mkIf cfg.enable {
    environment.plasma5.excludePackages = with pkgs.libsForQt5; [
      khelpcenter
      plasma-browser-integration
    ];
    environment.systemPackages = with pkgs; [
      spotify
      arandr
    ];
  };
}
