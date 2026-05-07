_: {
  flake.nixosModules.hardware-cpu-power =
    {
      config,
      lib,
      ...
    }:
    let
      isAmd = lib.elem "kvm-amd" config.boot.kernelModules;
    in
    {
      config = lib.mkIf config.environment.desktop.enable {
        boot.kernelParams = lib.mkIf isAmd [ "amd_pstate=guided" ];

        powerManagement = {
          cpuFreqGovernor = lib.mkDefault "powersave";
        };

        systemd.tmpfiles.rules = [
          "w /sys/devices/system/cpu/cpufreq/boost - - - - 0"
          "w /sys/devices/system/cpu/cpu*/cpufreq/energy_performance_preference - - - - power"
        ];
      };
    };
}
