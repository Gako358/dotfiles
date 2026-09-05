{ lib, modulesPath, ... }:

let
  installation = import ./_installation.nix;
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "ohci_pci"
    "ehci_pci"
    "usb_storage"
    "usbhid"
    "sd_mod"
    "sr_mod"
  ];

  fileSystems = lib.mkIf (!installation.ready) {
    "/" = {
      device = "none";
      fsType = "tmpfs";
    };
    "/persist" = {
      device = "none";
      fsType = "tmpfs";
    };
  };
}
