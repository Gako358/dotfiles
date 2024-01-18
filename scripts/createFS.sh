#!/usr/bin/env bash

declare -A disk_layouts

# Define your disk layouts
disk_layouts["desktop"]=( "nvme0n1:efi:512MiB"
                          "nvme0n1:swap:512MiB:37GiB"
                          "nvme0n1:nixos:37GiB:100%"
                          "nvme1n1:nix:0%:100%"
                          "sda:arch:0%:100%"
                          "sdb:opt:0%:100%" )

disk_layouts["laptop"]=( "sda:efi:0%:512MiB"
                         "sda:swap:512MiB:9.1GiB"
                         "sda:nixos:9.1GiB:100%" )

# Partition mount options based on filesystem
declare -A MOUNT_OPTIONS=(
  [btrfs]="noatime,compress=zstd,ssd,space_cache=v2"
  [ext4]="defaults"
  [xfs]="defaults"
  [fat]="defaults"
)

# Function to create partition
create_partition() {
  disk=$1
  partname=$2
  start=$3
  end=$4

  echo "Creating partition $partname on $disk from $start to $end"
  parted --script -a optimal "/dev/$disk" "mkpart $partname $start $end"
  [ "$partname" == "efi" ] && parted --script -a optimal "/dev/$disk" "set 1 boot on"
}

# Function to setup a specific layout
setup_layout() {
  layout_name=$1
  layout=${disk_layouts[$layout_name]}

  for partition in "${layout[@]}"; do
    IFS=':' read -ra ADDR <<< "$partition"
    create_partition "${ADDR[0]}" "${ADDR[1]}" "${ADDR[2]}" "${ADDR[3]}"
  done

  echo "Partitions for $layout_name created"
  lsblk

  # Add your mounting logic here based on $layout_name
  if [ "$layout_name" == "desktop" ]; then
    # For example
    mount_partition "btrfs" "/dev/nvme0n1p3" "/mnt" "root"
  elif [ "$layout_name" == "laptop" ]; then
    # For example
    mount_partition "btrfs" "/dev/sda3" "/mnt" "root"
  fi
}

function mount_partition {
  # Usage: mount_partition <filesystem> <device> <mountpoint> <subvolume> <options>

  local fs="$1"
  local device="$2"
  local mountpoint="$3"
  local subvolume="$4"
  local options="$5"

  local mount_cmd="mount"

  # Prepare mount command based on filesystem
  case $fs in
    btrfs)
      mount_cmd+=" -o ${MOUNT_OPTIONS[btrfs]}"
      if [[ $subvolume ]]; then
        mount_cmd+=",subvol=$subvolume"
      fi
      ;;
    ext4)
      mount_cmd+=" -o ${MOUNT_OPTIONS[ext4]}"
      ;;
    xfs)
      mount_cmd+=" -o ${MOUNT_OPTIONS[xfs]}"
      ;;
    fat)
      mount_cmd+=" -o ${MOUNT_OPTIONS[fat]}"
      ;;
    *)
      echo "Filesystem $fs not supported."
      exit 1
  esac

  # Add user provided options if any
  if [[ $options ]]; then
    mount_cmd+=",$options"
  fi

  # Execute mount command
  $mount_cmd $device $mountpoint
}
