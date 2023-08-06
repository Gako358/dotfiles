{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.lock;
in {
  options.services = {
    lock = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable screenlock service";
      };
    };
  };

  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    programs.slock.enable = true;
    programs.xss-lock.enable = true;
    programs.xss-lock.lockerCommand = "/run/wrappers/bin/slock";
  };
}
