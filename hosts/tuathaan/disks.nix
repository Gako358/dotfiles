{ pkgs
, config
, lib
, ...
}: {
  fileSystems."/" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [ "subvol=root" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/home" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [ "subvol=home" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/nix" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [ "subvol=nix" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/var" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [ "subvol=var" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/tmp" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [ "subvol=tmp" "noatime" "compress=zstd" "ssd" ];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/sda1";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/sda2"; }
  ];
}
