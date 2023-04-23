{
  pkgs,
  config,
  lib,
  ...
}: {
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/637c6913-61bb-4a02-ac38-7d386153ca5d";
    fsType = "btrfs";
    options = ["subvol=root" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/637c6913-61bb-4a02-ac38-7d386153ca5d";
    fsType = "btrfs";
    options = ["subvol=home" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/637c6913-61bb-4a02-ac38-7d386153ca5d";
    fsType = "btrfs";
    options = ["subvol=nix" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/637c6913-61bb-4a02-ac38-7d386153ca5d";
    fsType = "btrfs";
    options = ["subvol=var" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/tmp" = {
    device = "/dev/disk/by-uuid/637c6913-61bb-4a02-ac38-7d386153ca5d";
    fsType = "btrfs";
    options = ["subvol=tmp" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/10BB-82D1";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/88668115-cf94-435e-b0d3-80b576abe625";}
  ];
}
