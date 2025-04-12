{ modulesPath
, config
, lib
, ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [ "amdgpu" ];
      postResumeCommands = lib.mkAfter ''
        mkdir -p /btrfs_tmp
        mount -o subvolid=5 /dev/mapper/crypted_root /btrfs_tmp
        mkdir -p /btrfs_tmp/persist/snapshots/root

        if [[ -e /btrfs_tmp/root ]]; then
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/persist/snapshots/root/$timestamp"
        fi

        btrbk run

        btrfs subvolume create /btrfs_tmp/root
        umount /btrfs_tmp
      '';
    };
    kernelModules = [ "kvm-amd" ];
    # kernelParams = [ "quiet" "splash" ];
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      systemd-boot.enable = true;
    };
    # plymouth.enable = true;
  };
  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
