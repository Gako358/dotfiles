{ modulesPath, ... }:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
  boot = {
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "thunderbolt"
        "vmd"
        "nvme"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
    };
    kernelModules = [
      "kvm-intel"
      "virtio"
      "bluetooth"
      "btusb"
    ];
  };
  # Thunderbolt, a userspace daemon to enable security levels for Thunderbolt 3 on GNU/Linux.
  services.hardware.bolt.enable = true;
}
