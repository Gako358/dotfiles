{ config, pkgs, lib, ... }: {
  imports = [
    ./graphics.nix
    ./network.nix
  ];
  boot = {
    consoleLogLevel = 3;
    initrd = {
      verbose = false;
      postResumeCommands = lib.mkAfter ''
        mkdir -p /btrfs_tmp
        mount -o subvolid=5 /dev/mapper/crypted_root /btrfs_tmp
        mkdir -p /btrfs_tmp/persist/snapshots/root

        if [[ -e /btrfs_tmp/root ]]; then
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%d_%H:%M:%S")
            mv /btrfs_tmp/root "/btrfs_tmp/persist/snapshots/root/$timestamp"
        fi

        delete_subvolume_recursively() {
            local subvol="$1"
            IFS=$'\n'
            for i in $(btrfs subvolume list -o "$subvol" | cut -f 9- -d ' '); do
                delete_subvolume_recursively "/btrfs_tmp/$i"
            done
            btrfs subvolume delete "$subvol"
        }

        find /btrfs_tmp/persist/snapshots/root -maxdepth 1 -mtime +30 -type d | while read snapshot; do
            delete_subvolume_recursively "$snapshot"
        done

        btrfs subvolume create /btrfs_tmp/root
        umount /btrfs_tmp
      '';
    };
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.systemd.show_status=auto"
    ];
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      systemd-boot.enable = true;
    };
    plymouth = {
      enable = true;
      theme = "nixos-bgrt";
      themePackages = [ pkgs.nixos-bgrt-plymouth ];
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    enableAllFirmware = true;
    keyboard.zsa.enable = true;
  };

  nixpkgs = {
    config.allowUnfree = true;
    hostPlatform = lib.mkDefault "x86_64-linux";
  };
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  system.stateVersion = "24.11";
}
