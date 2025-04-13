{ modulesPath
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
  };
}
