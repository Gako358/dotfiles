{
  config,
  lib,
  ...
}: {
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware = {
    opengl.driSupport = true;
    bluetooth.enable = true;
    # Enable braodcom chip for bluetooth
    enableAllFirmware = true;
  };
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # Set your system kind (needed for flakes)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
