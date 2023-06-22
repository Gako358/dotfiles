{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.secrets;
in {
  options.services = {
    secrets = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable keyring services";
      };
    };
  };

  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    services = {
      dbus.packages = [pkgs.gnome.gnome-keyring pkgs.gcr];
      gnome.gnome-keyring = {
        enable = true;
      };
    };
    # Enable gnome-keyring in PAM
    security.pam.services.login.enableGnomeKeyring = true;
  };
}
