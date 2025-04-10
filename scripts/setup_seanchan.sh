#!/usr/bin/env bash

DEVICE_NAME="/dev/vda"
EFI_SIZE="512MiB"
LABEL_NAME="NIXOS"

function create_partitions {
  # Create partitions
  parted --script -a optimal "${DEVICE_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE_NAME}" mkpart efi 0% "${EFI_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" mkpart nixos "${EFI_SIZE}" 100%
  parted --script -a optimal "${DEVICE_NAME}" set 1 boot on

  echo "Partitions created"

  # Print table
  lsblk
}

function setup_filesystems {
  # Format EFI partition
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}1"
  mkfs.btrfs -L "${LABEL_NAME}" -f "${DEVICE_NAME}2"

  echo "Partitions formatted"

  # Mount the btrfs partition temporarily for subvolume creation
  mount "${DEVICE_NAME}2" /mnt

  # Create BTRFS subvolumes
  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/persist
  btrfs subvolume create /mnt/home

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "${DEVICE_NAME}2" /mnt

  mkdir -p /mnt/{boot/efi,nix,persist,etc/nixos,var/log,home}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "${DEVICE_NAME}2" /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=persist "${DEVICE_NAME}2" /mnt/persist
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "${DEVICE_NAME}2" /mnt/home

  # Mount EFI partition
  mount "${DEVICE_NAME}1" /mnt/boot/efi

  # Create directories for persistent data
  mkdir -p /mnt/persist/var/log
  mkdir -p /mnt/persist/etc/nixos

  # Bind mount the persistent directories
  mount -o bind /mnt/persist/etc/nixos /mnt/etc/nixos
  mount -o bind /mnt/persist/var/log /mnt/var/log

  echo "Filesystems set up with BTRFS subvolumes:"
  echo ""
  df -Th
  free -h
  echo "Run: nixos-install --flake .#seanchan --no-root-password"
}

create_partitions
setup_filesystems

echo ""
echo "Setup complete!"
echo ""
