{
  config,
  lib,
  ...
}: let
  cfg = config.services.lock;
in {
  options.services.lock.enable = lib.mkEnableOption "lock";
  config = lib.mkIf cfg.enable {
    programs.slock.enable = true;
    programs.xss-lock.enable = true;
    programs.xss-lock.lockerCommand = "/run/wrappers/bin/slock";
  };
}
