# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = ["wl" "amdgpu"];
  boot.kernelModules = ["kvm-amd" "wl"];
  boot.extraModulePackages = [config.boot.kernelPackages.broadcom_sta];

  fileSystems."/" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=root" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/home" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=home" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/tmp" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=tmp" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/nix" = {
    device = "/dev/nvme1n1p1";
    fsType = "btrfs";
    options = ["noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/nvme0n1p2";}
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.docker0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;
  # networking.interfaces.wwan0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
