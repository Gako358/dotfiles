{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.keyring;
in {
  options = {
    services.keyring = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the keyring service.";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm" || config.desktop.environment == "bspwm")) {
    home.packages = with pkgs; [
      gnome.seahorse
    ];
  };
}
