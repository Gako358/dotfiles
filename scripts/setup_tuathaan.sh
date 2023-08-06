#!/usr/bin/env bash

DEVICE_NAME="/dev/nvme0n1"
EFI_SIZE="512MiB"
SWAP_SIZE="9.1GiB"
LABEL_NAME="NIXOS"

function create_partitions {
  #Create partitions
  parted --script -a optimal "${DEVICE_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE_NAME}" mkpart efi 0% "${EFI_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" mkpart swap "${EFI_SIZE}" "${SWAP_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" mkpart nixos "${SWAP_SIZE}" 100%
  parted --script -a optimal "${DEVICE_NAME}" set 1 boot on

  clear
  echo "Partitions created"

  # Print table
  lsblk
}

function mount_partitions {
  # Format Partitions
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}p1"
  mkswap -L SWAP "${DEVICE_NAME}p2"
  mkfs.btrfs -L "${LABEL_NAME}" -f "${DEVICE_NAME}p3"

  echo "Partitions formatted"

  # Mount partitions
  mount "${DEVICE_NAME}p3" /mnt

  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/var
  btrfs subvolume create /mnt/tmp

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "${DEVICE_NAME}p3" /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix,var}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "${DEVICE_NAME}p3" /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "${DEVICE_NAME}p3" /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=var "${DEVICE_NAME}p3" /mnt/var
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp "${DEVICE_NAME}p3" /mnt/tmp

  mount "${DEVICE_NAME}p1" /mnt/boot/efi
  swapon "${DEVICE_NAME}p2"

  clear
  df -Th
  free -h
}

create_partitions
mount_partitions
