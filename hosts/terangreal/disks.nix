{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; {
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

  fileSystems."/arch" = {
    device = "/dev/disk/by-uuid/a2b994e1-5463-4ae6-a472-aa43c5ed595a";
    fsType = "ext4";
  };

  fileSystems."/opt" = {
    device = "/dev/disk/by-uuid/8edc5eb4-5fff-4a2e-af14-db40a2c7c35e";
    fsType = "xfs";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/nvme0n1p2";}
  ];
}
