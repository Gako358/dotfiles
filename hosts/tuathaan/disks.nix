{
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

  fileSystems."/nix" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=nix" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/var" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=var" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/tmp" = {
    device = "/dev/nvme0n1p3";
    fsType = "btrfs";
    options = ["subvol=tmp" "noatime" "compress=zstd" "ssd"];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/nvme0n1p2";}
  ];
}
