{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "kde") {
    environment.sessionVariables = {
      NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (pkgs.lib.reverseList config.environment.profiles)}";
    };
    services.xserver = {
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;
    };
  };
}
