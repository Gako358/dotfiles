{
  lib,
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
      gnome.gnome-keyring = {
        enable = true;
      };
    };
  };
}
