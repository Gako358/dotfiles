#!/usr/bin/env bash

function mkfs_desktop
{
  #Create partitions
  parted --script -a optimal /dev/nvme0n1 'mklabel gpt' & pid1=$!
  wait $pid1
  parted --script -a optimal /dev/nvme0n1 'mkpart efi 0% 512MiB' & pid2=$!
  wait $pid2
  parted --script -a optimal /dev/nvme0n1 'mkpart swap 512MiB 37GiB' & pid3=$!
  wait $pid3
  parted --script -a optimal /dev/nvme0n1 'mkpart nixos 37GiB 100%' & pid4=$!
  wait $pid4
  parted --script -a optimal /dev/nvme0n1 'set 1 boot on' & pid5=$!
  wait $pid5
  parted --script -a optimal /dev/nvme1n1 'mklabel gpt' & pid6=$!
  wait $pid6
  parted --script -a optimal /dev/nvme1n1 'mkpart nix 0% 100%' & pid7=$!
  wait $pid7
  parted --script -a optimal /dev/sda 'mklabel gpt' & pid8=$!
  wait $pid8
  parted --script -a optimal /dev/sda 'mkpart arch 0% 100%' & pid9=$!
  wait $pid9
  parted --script -a optimal /dev/sdb 'mklabel gpt' & pid10=$!
  wait $pid10
  parted --script -a optimal /dev/sdb 'mkpart opt 0% 100%' & pid11=$!
  wait $pid11

  clear
  echo "Partitions created"

  # Print table
  lsblk
}

function mkfs_desktop_simple
{
  Create partitions, without sda1 and sdb1
  parted --script -a optimal /dev/nvme0n1 'mklabel gpt' & pid1=$!
  wait $pid1
  parted --script -a optimal /dev/nvme0n1 'mkpart efi 0% 512MiB' & pid2=$!
  wait $pid2
  parted --script -a optimal /dev/nvme0n1 'mkpart swap 512MiB 37GiB' & pid3=$!
  wait $pid3
  parted --script -a optimal /dev/nvme0n1 'mkpart nixos 37GiB 100%' & pid4=$!
  wait $pid4
  parted --script -a optimal /dev/nvme0n1 'set 1 boot on' & pid5=$!
  wait $pid5
  parted --script -a optimal /dev/nvme1n1 'mklabel gpt' & pid6=$!
  wait $pid6
  parted --script -a optimal /dev/nvme1n1 'mkpart nix 0% 100%' & pid7=$!
  wait $pid7

  clear
  echo "Partitions created"

  # Print table
  lsblk

}

function mkfs_laptop
{
  #Create partitions
  parted --script -a optimal /dev/sda 'mklabel gpt' & pid1=$!
  wait $pid1
  parted --script -a optimal /dev/sda 'mkpart efi 0% 512MiB' & pid2=$!
  wait $pid2
  parted --script -a optimal /dev/sda 'mkpart swap 512MiB 9.1GiB' & pid3=$!
  wait $pid3
  parted --script -a optimal /dev/sda 'mkpart nixos 9.1GiB 100%' & pid4=$!
  wait $pid4
  parted --script -a optimal /dev/sda 'set 1 boot on' & pid5=$!
  wait $pid5

  clear
  echo "Partitions created"

  # Print table
  lsblk
}

function setup_desktop
{
  # Format Partitions
  mkfs.fat -F32 -n EFI /dev/nvme0n1p1 & pid12=$!
  wait $pid12
  mkswap -L SWAP /dev/nvme0n1p2 & pid13=$!
  wait $pid13
  mkfs.btrfs -L NIXOS -f /dev/nvme0n1p3 & pid14=$!
  wait $pid14
  mkfs.btrfs -L NIX -f /dev/nvme1n1p1 & pid15=$!
  wait $pid15
  mkfs.ext4 -L ARCH /dev/sda1 & pid16=$!
  wait $pid16
  mkfs.xfs -L OPT -f /dev/sdb1 & pid17=$!
  wait $pid17

  echo "Partitions formatted"

  # Mount partitions
  mount /dev/nvme0n1p3 /mnt

  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/tmp

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root /dev/nvme0n1p3 /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix,opt,arch}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home /dev/nvme0n1p3 /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp /dev/nvme0n1p3 /mnt/tmp

  mount -o noatime,compress=zstd,ssd,space_cache=v2 /dev/nvme1n1p1 /mnt/nix
  mount /dev/sda1 /mnt/arch
  mount /dev/sdb1 /mnt/opt

  mount /dev/nvme0n1p1 /mnt/boot/efi
  swapon /dev/nvme0n1p2 & pid18=$!
  wait $pid18

  clear
  df -Th
  free -h
  echo "Run: nixos-install --flake github:gako358/Dotfiles#mXdesktop" 
}

function setup_desktop_simple
{
  # Format Partitions
  mkfs.fat -F32 -n EFI /dev/nvme0n1p1 & pid12=$!
  wait $pid12
  mkswap -L SWAP /dev/nvme0n1p2 & pid13=$!
  wait $pid13
  mkfs.btrfs -L NIXOS -f /dev/nvme0n1p3 & pid14=$!
  wait $pid14
  mkfs.btrfs -L NIX -f /dev/nvme1n1p1 & pid15=$!
  wait $pid15

  echo "Partitions formatted"

  # Mount partitions
  mount /dev/nvme0n1p3 /mnt

  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/tmp

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root /dev/nvme0n1p3 /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix,opt,arch}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home /dev/nvme0n1p3 /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp /dev/nvme0n1p3 /mnt/tmp

  mount -o noatime,compress=zstd,ssd,space_cache=v2 /dev/nvme1n1p1 /mnt/nix
  mount /dev/sda1 /mnt/arch
  mount /dev/sdb1 /mnt/opt

  mount /dev/nvme0n1p1 /mnt/boot/efi
  swapon /dev/nvme0n1p2 & pid18=$!
  wait $pid18

  clear
  df -Th
  free -h
  echo "Run: nixos-install --flake github:gako358/Dotfiles#mXdesktop" 
}

function setup_laptop
{
  # Format Partitions
  mkfs.fat -F32 -n EFI /dev/sda1 & pid6=$!
  wait $pid6
  mkswap -L SWAP /dev/sda2 & pid7=$!
  wait $pid7
  mkfs.btrfs -L NIXOS -f /dev/sda3 & pid8=$!
  wait $pid8

  echo "Partitions formatted"

  # Mount partitions
  mount /dev/sda3 /mnt

  btrfs subvolume create /mnt/root
  btrfs subvolume create /mnt/home
  btrfs subvolume create /mnt/nix
  btrfs subvolume create /mnt/var
  btrfs subvolume create /mnt/tmp

  umount /mnt
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=root /dev/sda3 /mnt

  mkdir -p /mnt/{boot/efi,home,tmp,nix,var}
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=home /dev/sda3 /mnt/home
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=nix /dev/sda3 /mnt/nix
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=var /dev/sda3 /mnt/var
  mount -o noatime,compress=zstd,ssd,space_cache=v2,subvol=tmp /dev/sda3 /mnt/tmp

  mount /dev/sda1 /mnt/boot/efi
  swapon /dev/sda2 & pid9=$!
  wait $pid9

  clear
  df -Th
  free -h
  echo "Run: nixos-install --flake github:gako358/Dotfiles#mXlaptop" 
}

function install_desktop() {
  # Install
  nixos-install --flake github:gako358/nixos#terangreal
}

function install_laptop() {
  # Install
  nixos-install --flake github:gako358/nixos#tuathaan
}

"$@"
