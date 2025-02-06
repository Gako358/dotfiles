{
  modulesPath,
  config,
  lib,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
    initrd = {
      availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
      kernelModules = ["amdgpu"];
      luks.devices = {
        cryptroot = {
          device = "/dev/nvme0n1p3";
          preLVM = true;
        };
        cryptswap = {
          device = "/dev/nvme0n1p2";
        };
      };
    };
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
  };

  fileSystems."/" = {
    device = "/dev/mapper/cryptroot";
    fsType = "btrfs";
    options = ["subvol=root" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/home" = {
    device = "/dev/mapper/cryptroot";
    fsType = "btrfs";
    options = ["subvol=home" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/tmp" = {
    device = "/dev/mapper/cryptroot";
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
    {device = "/dev/mapper/cryptswap";}
  ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
