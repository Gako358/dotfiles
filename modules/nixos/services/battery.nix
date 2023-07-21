{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.battery;
in {
  options.services.battery.enable = lib.mkEnableOption "Enable battery services for laptops";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      acpilight
    ];

    services.tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "powersave";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        # Prevents battery from charging above 90%
        START_CHARGE_THRESH_BAT0 = "40";
        STOP_CHARGE_THRESH_BAT0 = "90";

        # Limit CPU frequency
        CPU_MAX_PERF_ON_AC = "80";
        CPU_MAX_PERF_ON_BAT = "70";
      };
    };
  };
}
