{ pkgs
, config
, lib
, ...
}:
with lib;
with builtins; {
  fileSystems."/" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = [ "subvol=root" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/home" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = [ "subvol=home" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/tmp" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = [ "subvol=tmp" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/nix" = {
    device = "/dev/nvme1n1p1";
    fsType = "btrfs";
    options = [ "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/arch" = {
    device = "/dev/sdb1";
    fsType = "ext4";
  };

  fileSystems."/opt" = {
    device = "/dev/sda1";
    fsType = "xfs";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/nvme0n1p2"; }
  ];
}
