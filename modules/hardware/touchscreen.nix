_: {
  flake.nixosModules.hardware-touchscreen =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.system.touchscreen;
    in
    {
      options.system.touchscreen = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable touchscreen support (HID/I2C kernel drivers + libinput).";
        };

        extraKernelModules = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          example = [
            "elants_i2c"
            "silead"
          ];
          description = "Additional vendor-specific touchscreen controller modules to load.";
        };
      };

      config = lib.mkIf cfg.enable {
        boot.kernelModules = [
          "hid_multitouch"
          "i2c_hid"
          "i2c_hid_acpi"
          "intel_lpss"
          "intel_lpss_pci"
        ]
        ++ cfg.extraKernelModules;

        environment.systemPackages = with pkgs; [
          libinput
          evtest
        ];
      };
    };
}
