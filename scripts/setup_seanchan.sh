#!/usr/bin/env bash

DEVICE_NAME="/dev/vda"
EFI_SIZE="512MiB"
SWAP_SIZE="8G"
CRYPTED_MAPPER_NAME="crypted"
LABEL_NAME="NIXOS"

function create_partitions {
  # Create partitions
  echo "Creating partitions on ${DEVICE_NAME}..."
  parted --script -a optimal "${DEVICE_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE_NAME}" mkpart ESP fat32 1MiB "${EFI_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" set 1 boot on
  parted --script -a optimal "${DEVICE_NAME}" mkpart primary "${EFI_SIZE}" 100%

  # Print partition table
  echo "Partition table created:"
  lsblk "${DEVICE_NAME}"
}

function setup_luks {
  echo "Setting up LUKS encryption on ${DEVICE_NAME}2..."
  cryptsetup luksFormat --type luks2 "${DEVICE_NAME}2"
  cryptsetup luksOpen "${DEVICE_NAME}2" "${CRYPTED_MAPPER_NAME}"

  echo "LUKS encryption set up successfully"
}

function setup_filesystems {
  echo "Formatting EFI partition..."
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}1"

  echo "Creating BTRFS filesystem on encrypted device..."
  mkfs.btrfs -L "${LABEL_NAME}" "/dev/mapper/${CRYPTED_MAPPER_NAME}"

  mount "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt

  echo "Creating BTRFS subvolumes..."
  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/persist
  btrfs subvolume create /mnt/swap

  umount /mnt

  echo "Mounting BTRFS subvolumes..."
  mount -o noatime,compress=zstd,subvol=root "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt

  mkdir -p /mnt/{boot/efi,nix,home,persist,var/log,swap}
  mount -o noatime,compress=zstd,subvol=nix "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt/nix
  mount -o noatime,compress=zstd,subvol=home "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt/home
  mount -o noatime,compress=zstd,subvol=persist "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt/persist
  mount -o noatime,compress=no,subvol=swap "/dev/mapper/${CRYPTED_MAPPER_NAME}" /mnt/swap

  mount "${DEVICE_NAME}1" /mnt/boot/efi

  # Create directories for persistent data
  mkdir -p /mnt/persist/var/log
  mount -o bind /mnt/persist/var/log /mnt/var/log

  echo "Filesystem setup complete"
}

function create_swapfile {
  echo "Creating swapfile..."

  truncate -s 0 /mnt/swap/swapfile
  chattr +C /mnt/swap/swapfile

  dd if=/dev/zero of=/mnt/swap/swapfile bs=1M count=$((${SWAP_SIZE%G} * 1024)) status=progress

  chmod 600 /mnt/swap/swapfile
  mkswap /mnt/swap/swapfile
  swapon /mnt/swap/swapfile

  echo "Swapfile created and activated"
  free -h
}

function display_summary {
  echo "Filesystem layout:"
  df -Th | grep /mnt
  echo "Mounted subvolumes:"
  findmnt -t btrfs
  echo "Disk usage:"
  btrfs filesystem usage /mnt
}

echo "Starting NixOS disk setup with LUKS encryption and BTRFS..."
echo "WARNING: This will erase all data on ${DEVICE_NAME}!"
read -p "Continue? (y/N): " confirm
if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
  echo "Aborting..."
  exit 1
fi

create_partitions
setup_luks
setup_filesystems
create_swapfile
display_summary

echo ""
echo "Setup complete! Ready for NixOS installation."
echo "Next steps:"
echo "Install: nixos-install --flake .#seanchan --no-root-password"
