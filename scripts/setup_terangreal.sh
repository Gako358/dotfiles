#!/usr/bin/env bash

DEVICE1_NAME="/dev/nvme0n1"
DEVICE2_NAME="/dev/nvme1n1"
DEVICE3_NAME="/dev/sda"
DEVICE4_NAME="/dev/sdb"

function create_partitions_desktop {
  # Create partitions for DEVICE1_NAME
  parted --script -a optimal "${DEVICE1_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE1_NAME}" mkpart efi 0% 512MiB
  parted --script -a optimal "${DEVICE1_NAME}" mkpart swap 512MiB 37GiB
  parted --script -a optimal "${DEVICE1_NAME}" mkpart nixos 37GiB 100%
  parted --script -a optimal "${DEVICE1_NAME}" set 1 boot on

  # Create partitions for DEVICE2_NAME
  parted --script -a optimal "${DEVICE2_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE2_NAME}" mkpart nix 0% 100%

  # Create partitions for DEVICE3_NAME
  parted --script -a optimal "${DEVICE3_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE3_NAME}" mkpart arch 0% 100%

  # Create partitions for DEVICE4_NAME
  parted --script -a optimal "${DEVICE4_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE4_NAME}" mkpart opt 0% 100%

  clear
  echo "Partitions created"

  # Print table
  lsblk
}

function setup_desktop {
  # Format Partitions
  mkfs.fat -F32 -n EFI "${DEVICE1_NAME}p1"
  mkswap -L SWAP "${DEVICE1_NAME}p2"
  mkfs.btrfs -L NIXOS -f "${DEVICE1_NAME}p3"
  mkfs.btrfs -L NIX -f "${DEVICE2_NAME}p1"
  mkfs.ext4 -L ARCH "${DEVICE3_NAME}1"
  mkfs.xfs -L OPT -f "${DEVICE4_NAME}1"

  echo "Partitions formatted"

  # Mount partitions
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "${DEVICE1_NAME}p3" /mnt
  btrfs subvolume create /mnt/{root,home,tmp}
  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "${DEVICE1_NAME}p3" /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix,opt,arch}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "${DEVICE1_NAME}p3" /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp "${DEVICE1_NAME}p3" /mnt/tmp
  mount -o noatime,compress=zstd,ssd,space_cache=v2 "${DEVICE2_NAME}p1" /mnt/nix
  mount "${DEVICE3_NAME}1" /mnt/arch
  mount "${DEVICE4_NAME}1" /mnt/opt
  mount "${DEVICE1_NAME}p1" /mnt/boot/efi
  swapon "${DEVICE1_NAME}p2"

  clear
  df -Th
  free -h
  echo "Run: nixos-install --flake .#terangreal"
}

create_partitions_desktop
setup_desktop
