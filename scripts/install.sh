#!/usr/bin/env bash

function create_partitions() {
  local device=$1
  local efi_size=$2
  local swap_size=$3
  local nixos_size=$4
  local boot_label=$5
  local swap_label=$6
  local nixos_label=$7
  local nix_label=$8
  local additional_devices=$9

  # Create partitions
  parted --script -a optimal "$device" 'mklabel gpt'
  parted --script -a optimal "$device" "mkpart $boot_label 0% $efi_size"
  parted --script -a optimal "$device" "mkpart $swap_label $efi_size $swap_size"
  parted --script -a optimal "$device" "mkpart $nixos_label $swap_size $nixos_size"
  parted --script -a optimal "$device" 'set 1 boot on'

  # Format partitions
  mkfs.fat -F32 -n EFI "${device}p1"
  mkswap -L SWAP "${device}p2"
  mkfs.btrfs -L NIXOS -f "${device}p3"

  if [ -n "$nix_label" ]; then
    local nix_device="$9"
    parted --script -a optimal "$nix_device" "mkpart $nix_label $nixos_size 100%"
    mkfs.btrfs -L NIX -f "${nix_device}p1"
  fi

  if [ -n "$additional_devices" ]; then
    for partition in "${additional_devices[@]}"; do
      parted --script -a optimal "${partition[4]}" "mkpart ${partition[0]} ${partition[1]} ${partition[2]}"
      mkfs.btrfs -L "${partition[0]}" -f "${partition[4]}p${partition[3]}"
    done
  fi
}

function mount_partitions() {
  local device=$1
  local nix_label=$2
  local nix_device=$3
  shift 3
  local additional_partitions=("$@")
  local nixos_subvolumes=(root home tmp)
  local mount_options="noatime,compress=zstd,ssd,space_cache=v2"

  mount "${device}p3" /mnt

  for subvol in "${nixos_subvolumes[@]}"; do
    btrfs subvolume create "/mnt/$subvol"
  done

  umount /mnt

  mount -o "$mount_options",subvol=root "${device}p3" /mnt

  for subvol in "${nixos_subvolumes[@]}"; do
    mkdir -p "/mnt/$subvol"
    mount -o "$mount_options",subvol="$subvol" "${device}p3" "/mnt/$subvol"
  done

  mkdir -p /mnt/boot/efi
  mount "${device}p1" /mnt/boot/efi
  swapon "${device}p2"

  if [ -n "$nix_label" ]; then
    mkdir -p /mnt/nix
    mount -o "$mount_options" "${nix_device}p1" /mnt/nix
  fi

  if [ -n "$additional_partitions" ]; then
    for partition_str in "${additional_partitions[@]}"; do
      IFS=',' read -ra partition <<< "$partition_str"
      local mount_point="/mnt/${partition[0]}"
      mkdir -p "$mount_point"
      mount -o "$mount_options" "${partition[4]}p${partition[3]}" "$mount_point"
    done
  fi

  clear
  df -Th
  free -h

  echo "Partitions created and formatted"
}

function setup_desktop() {
  nix_label="nix"
  additional_devices=(("opt" "0%" "100%" "1" "/dev/sda"), ("arch" "0%" "100%" "1" "/dev/sdb"))
  create_partitions /dev/nvme0n1 512MiB 37GiB 100% efi swap nixos nix /dev/nvme1n1 additional_devices
  mount_partitions /dev/nvme0n1 nix /dev/nvme1n1 "${additional_devices[@]}"
  echo "Run: nixos-install --flake github:gako358/Dotfiles#terangreal"
}

function setup_worklaptop() {
  create_partitions /dev/nvme0n1 512MiB 9.1GiB 100% efi swap nixos
  mount_partitions /dev/nvme0n1
  echo "Run: nixos-install --flake github:gako358/Dotfiles#tuathaan"
}

function setup_homelaptop() {
  create_partitions /dev/sda 512MiB 9.1GiB 100% efi swap nixos
  mount_partitions /dev/sda
  echo "Run: nixos-install --flake github:gako358/Dotfiles#tuathaan"
}

"$@"
