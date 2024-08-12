#!/usr/bin/env bash

DEVICE_NAME="/dev/nvme0n1"
EFI_SIZE="512MiB"
SWAP_SIZE="9.1GiB"
LABEL_NAME="NIXOS"
CRYPTROOT_NAME="cryptroot"
CRYPTSWAP_NAME="cryptswap"

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

function setup_luks {
    # Encrypt the root partition
    cryptsetup --verify-passphrase -v luksFormat "${DEVICE_NAME}p3"
    cryptsetup open "${DEVICE_NAME}p3" "${CRYPTROOT_NAME}"

    # Encrypt the swap partition
    cryptsetup --verify-passphrase -v luksFormat "${DEVICE_NAME}p2"
    cryptsetup open "${DEVICE_NAME}p2" "${CRYPTSWAP_NAME}"

    echo "LUKS setup completed"
}

function mount_partitions {
    # Format Partitions
    mkfs.fat -F32 -n EFI "${DEVICE_NAME}p1"
    mkswap -L SWAP "/dev/mapper/${CRYPTSWAP_NAME}"
    mkfs.btrfs -L "${LABEL_NAME}" -f "/dev/mapper/${CRYPTROOT_NAME}"

    echo "Partitions formatted"

    # Mount partitions
    mount "/dev/mapper/${CRYPTROOT_NAME}" /mnt

    btrfs subvolume create /mnt/root
    btrfs subvolume create /mnt/home
    btrfs subvolume create /mnt/nix
    btrfs subvolume create /mnt/var
    btrfs subvolume create /mnt/tmp

    umount /mnt
    mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root "/dev/mapper/${CRYPTROOT_NAME}" /mnt

    mkdir -p /mnt/{boot/efi,home,tmp,nix,var}
    mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home "/dev/mapper/${CRYPTROOT_NAME}" /mnt/home
    mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix "/dev/mapper/${CRYPTROOT_NAME}" /mnt/nix
    mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=var "/dev/mapper/${CRYPTROOT_NAME}" /mnt/var
    mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp "/dev/mapper/${CRYPTROOT_NAME}" /mnt/tmp

    mount "${DEVICE_NAME}p1" /mnt/boot/efi
    swapon "/dev/mapper/${CRYPTSWAP_NAME}"

    clear
    df -Th
    free -h
    echo "Run: nixos-install --flake .#tuathaan"
}

create_partitions
setup_luks
mount_partitions
