#!/usr/bin/env bash

DEVICE_NAME="/dev/vda"
EFI_SIZE="512MiB"
SWAP_SIZE="1.9GiB"
LABEL_NAME="NIXOS"
CRYPTROOT_NAME="cryptroot"
CRYPTSWAP_NAME="cryptswap"

function create_partitions {
  # Create partitions
  parted --script -a optimal "${DEVICE_NAME}" mklabel gpt
  parted --script -a optimal "${DEVICE_NAME}" mkpart efi 0% "${EFI_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" mkpart swap "${EFI_SIZE}" "${SWAP_SIZE}"
  parted --script -a optimal "${DEVICE_NAME}" mkpart nixos "${SWAP_SIZE}" 100%
  parted --script -a optimal "${DEVICE_NAME}" set 1 boot on

  echo "Partitions created"

  # Print table
  lsblk
}

function setup_encryption {
  # Encrypt the root partition
  cryptsetup --verify-passphrase -v luksFormat "${DEVICE_NAME}3"
  cryptsetup open "${DEVICE_NAME}3" "${CRYPTROOT_NAME}"

  # Encrypt the swap partition
  cryptsetup --verify-passphrase -v luksFormat "${DEVICE_NAME}2"
  cryptsetup open "${DEVICE_NAME}2" "${CRYPTSWAP_NAME}"

  echo "LUKS setup completed"
}

function setup_filesystems {
  # Format EFI and swap partitions
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}1"
  mkswap -L SWAP "${DEVICE_NAME}2"
  mkfs.btrfs -L "${LABEL_NAME}" -f "/dev/mapper/${CRYPTROOT_NAME}"

  echo "Partitions formatted"

  # Mount the btrfs partition temporarily for subvolume creation
  mount "/dev/mapper/${CRYPTROOT_NAME}" /mnt

  # Create BTRFS subvolumes
  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/persist
  btrfs subvolume create /mnt/home

  # Unmount the temporary mount
  umount /mnt

  # Mount the root subvolume first
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=@ "/dev/mapper/${CRYPTROOT_NAME}" /mnt

  # Create necessary directories in the tmpfs root
  mkdir -p /mnt/{boot/efi,nix,persist,etc/nixos,var/log,home}

  # Mount BTRFS subvolumes
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "/dev/mapper/${CRYPTROOT_NAME}" /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=persist "/dev/mapper/${CRYPTROOT_NAME}" /mnt/persist
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "/dev/mapper/${CRYPTROOT_NAME}" /mnt/home

  # Mount EFI partition
  mount "${DEVICE_NAME}1" /mnt/boot/efi
  swapon "/dev/mapper/${CRYPTSWAP_NAME}"

  # Create directories for persistent data
  mkdir -p /mnt/persist/var/log
  mkdir -p /mnt/persist/etc/nixos

  # Bind mount the persistent directories
  mount -o bind /mnt/persist/etc/nixos /mnt/etc/nixos
  mount -o bind /mnt/persist/var/log /mnt/var/log

  echo "Filesystems set up with BTRFS subvolumes on encrypted LUKS container:"
  echo ""
  df -Th
  free -h
  echo "Run: nixos-install --flake .#seanchan --no-root-password"
}

create_partitions
setup_encryption
setup_filesystems

echo ""
echo "Setup complete!"
echo ""
