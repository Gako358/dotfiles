#!/usr/bin/env bash

DEVICE_NAME="/dev/vda"
EFI_SIZE="512MiB"
SWAP_SIZE="1.9GiB"
LABEL_NAME="NIXOS"

function create_partitions {
  # Create partitions
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

function setup_filesystems {
  # Format Partitions
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}p1"
  mkswap -L SWAP "${DEVICE_NAME}p2"
  mkfs.btrfs -L "${LABEL_NAME}" -f "${DEVICE_NAME}p3"

  echo "Partitions formatted"

  # Mount the btrfs partition temporarily for subvolume creation
  mount "${DEVICE_NAME}p3" /mnt

  # Create BTRFS subvolumes
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/persist
  btrfs subvolume create /mnt/home

  # Unmount the temporary mount
  umount /mnt

  # Mount root as tmpfs (ephemeral)
  mount -t tmpfs none /mnt

  # Create necessary directories in the tmpfs root
  mkdir -p /mnt/{boot/efi,nix,persist,etc/nixos,var/log,home}

  # Mount BTRFS subvolumes
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "${DEVICE_NAME}p3" /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=persist "${DEVICE_NAME}p3" /mnt/persist
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "${DEVICE_NAME}p3" /mnt/home

  # Mount EFI partition
  mount "${DEVICE_NAME}p1" /mnt/boot/efi

  # Create directories for persistent data
  mkdir -p /mnt/persist/var/log

  # Bind mount the persistent directories
  mount -o bind /mnt/persist/var/log /mnt/var/log

  # Enable swap
  swapon "${DEVICE_NAME}p2"

  clear
  echo "Filesystems set up with tmpfs root and BTRFS subvolumes:"
  echo ""
  echo "BTRFS subvolumes:"
  btrfs subvolume list /mnt/nix
  echo ""
  echo "Mount points:"
  df -Th | grep -v tmpfs | sort
  echo ""
  echo "Tmpfs mounts:"
  df -Th | grep tmpfs
  echo ""
  echo "Memory usage:"
  free -h
}

create_partitions
setup_filesystems
