#!/usr/bin/env bash

DEVICE_NAME="/dev/vda"
EFI_SIZE="512MiB"
SWAP_SIZE="1.9GiB"
LABEL_NAME="NIXOS"
LUKS_NAME="crypted"

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

function setup_encryption {
  # Encrypt the nixos partition
  echo "Setting up LUKS encryption on ${DEVICE_NAME}p3"
  echo "You will be prompted to enter and confirm a passphrase for encryption."

  # Format the partition with LUKS
  cryptsetup luksFormat "${DEVICE_NAME}p3"

  # Open the encrypted partition
  echo "Opening the encrypted partition"
  cryptsetup open "${DEVICE_NAME}p3" "${LUKS_NAME}"

  # Show the mapped device
  echo "Encrypted partition mapped to /dev/mapper/${LUKS_NAME}"
}

function setup_filesystems {
  # Format EFI and swap partitions
  mkfs.fat -F32 -n EFI "${DEVICE_NAME}p1"
  mkswap -L SWAP "${DEVICE_NAME}p2"

  # Format the encrypted partition with BTRFS
  mkfs.btrfs -L "${LABEL_NAME}" -f "/dev/mapper/${LUKS_NAME}"

  echo "Partitions formatted"

  # Mount the btrfs partition temporarily for subvolume creation
  mount "/dev/mapper/${LUKS_NAME}" /mnt

  # Create BTRFS subvolumes
  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/persist
  btrfs subvolume create /mnt/home

  # Unmount the temporary mount
  umount /mnt

  # Mount the root subvolume first
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=@ "/dev/mapper/${LUKS_NAME}" /mnt

  # Create necessary directories in the tmpfs root
  mkdir -p /mnt/{boot/efi,nix,persist,etc/nixos,var/log,home}

  # Mount BTRFS subvolumes
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "/dev/mapper/${LUKS_NAME}" /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=persist "/dev/mapper/${LUKS_NAME}" /mnt/persist
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "/dev/mapper/${LUKS_NAME}" /mnt/home

  # Mount EFI partition
  mount "${DEVICE_NAME}p1" /mnt/boot/efi

  # Create directories for persistent data
  mkdir -p /mnt/persist/var/log
  mkdir -p /mnt/persist/etc/nixos

  # Bind mount the persistent directories
  mount -o bind /mnt/persist/etc/nixos /mnt/etc/nixos
  mount -o bind /mnt/persist/var/log /mnt/var/log

  # Enable swap
  swapon "${DEVICE_NAME}p2"

  clear
  echo "Filesystems set up with tmpfs root and BTRFS subvolumes on encrypted LUKS container:"
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

function create_keyfile {
  echo "Creating a keyfile for automatic unlocking during boot..."

  mkdir -p /mnt/boot/keys
  dd if=/dev/urandom of=/mnt/boot/keys/luks-key bs=1 count=4096
  chmod 600 /mnt/boot/keys/luks-key

  cryptsetup luksAddKey "${DEVICE_NAME}p3" /mnt/boot/keys/luks-key
}

create_partitions
setup_encryption
setup_filesystems
create_keyfile

echo ""
echo "Setup complete!"
echo ""
