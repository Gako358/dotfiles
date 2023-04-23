{
  pkgs,
  config,
  lib,
  ...
}: {
  fileSystems."/" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };

  fileSystems."/home" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

  fileSystems."/nix" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=nix" ];
    };

  fileSystems."/var" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=var" ];
    };

  fileSystems."/tmp" =
    { device = "/dev/sda3";
      fsType = "btrfs";
      options = [ "subvol=tmp" ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/5681-D634";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/e9413a59-3ff0-4ce9-a286-149448ba230b"; }
    ];
}
