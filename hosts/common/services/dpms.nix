{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.xsetDPMS;
in {
  options = {
    services.xsetDPMS = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the DPMS service, xset s off -dpms on wakeup";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm")) {
    systemd.user.services.xsetDPMS = {
      description = "Setup noblank and dpms";
      after = ["sleep.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        ExecStart = "${pkgs.xorg.xset}/bin/xset -display :0 s off -dpms";
        RemainAfterExit = "yes";
        TimeoutSec = "5s";
        RestartSec = "5s";
        Restart = "on-failure";
      };
    };
  };
}
