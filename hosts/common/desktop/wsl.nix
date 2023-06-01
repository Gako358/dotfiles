{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "wsl") {
    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
  };
}
