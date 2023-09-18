#!/usr/bin/env bash

DEVICE1_NAME="/dev/nvme0n1"
DEVICE2_NAME="/dev/nvme1n1"

function create_partitions {
  # Create partitions for DEVICE1_NAME
  parted --script -a optimal "${DEVICE1_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE1_NAME}" mkpart efi 0% 512MiB
  parted --script -a optimal "${DEVICE1_NAME}" mkpart swap 512MiB 19GiB
  parted --script -a optimal "${DEVICE1_NAME}" mkpart nixos 19GiB 100%
  parted --script -a optimal "${DEVICE1_NAME}" set 1 boot on

  # Create partitions for DEVICE2_NAME
  parted --script -a optimal "${DEVICE2_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE2_NAME}" mkpart nix 0% 100%

  clear
  echo "Partitions created"

  # Print table
  lsblk
}

function mount_partitions {
  # Format Partitions
  mkfs.fat -F32 -n EFI "${DEVICE1_NAME}p1"
  mkswap -L SWAP "${DEVICE1_NAME}p2"
  mkfs.btrfs -L NIXOS -f "${DEVICE1_NAME}p3"
  mkfs.btrfs -L NIX -f "${DEVICE2_NAME}p1"

  echo "Partitions formatted"

  # Mount partitions
  mount "${DEVICE1_NAME}p3" /mnt

  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/tmp

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "${DEVICE1_NAME}p3" /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "${DEVICE1_NAME}p3" /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp "${DEVICE1_NAME}p3" /mnt/tmp
  mount -o noatime,compress=zstd,ssd,space_cache=v2 "${DEVICE2_NAME}p1" /mnt/nix
  mount "${DEVICE1_NAME}p1" /mnt/boot/efi
  swapon "${DEVICE1_NAME}p2"

  clear
  df -Th
  free -h
  echo "Run: nixos-install --flake .#terangreal"
}

create_partitions
mount_partitions
