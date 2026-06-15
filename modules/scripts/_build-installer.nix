{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  lsblk = "${pkgs.util-linux}/bin/lsblk";
  umount = "${pkgs.util-linux}/bin/umount";
  mount = "${pkgs.util-linux}/bin/mount";
  partprobe = "${pkgs.util-linux}/bin/partprobe";
  sgdisk = "${pkgs.gptfdisk}/bin/sgdisk";
  mkfsvfat = "${pkgs.dosfstools}/bin/mkfs.vfat";
  udevadm = "${pkgs.systemd}/bin/udevadm";
in
pkgs.writeShellScriptBin "build-installer" ''
  set -euo pipefail

  usage() {
    echo "Usage: build-installer <host> [/dev/sdX] [host-key-path]"
    echo
    echo "  Builds a NixOS installer ISO for <host>."
    echo "  With a USB device, also flashes it and writes the sops key partition"
    echo "  (master age key + the host's SSH key for a promptless reinstall)."
    exit 1
  }

  host="''${1:-}"
  dev="''${2:-}"
  hostkey_src="''${3:-}"
  [ -n "$host" ] || usage

  if root=$(${git} rev-parse --show-toplevel 2>/dev/null); then :; else root="$PWD"; fi
  cd "$root"

  if [ ! -d "modules/hosts/$host" ]; then
    echo "Unknown host '$host'. Available hosts:" >&2
    ls modules/hosts >&2
    exit 1
  fi

  echo ">> Building installer ISO for '$host' (this can take a while)..."
  out=$(nix build ".#nixosConfigurations.installer-$host.config.system.build.isoImage" \
    --no-link --print-out-paths)
  iso=$(echo "$out"/iso/*.iso)
  echo ">> ISO built: $iso"

  if [ -z "$dev" ]; then
    echo
    echo "No USB device given. To flash manually:"
    echo "  sudo dd if='$iso' of=/dev/sdX bs=4M status=progress oflag=sync"
    echo "Then re-run with the device to also write the sops key partition:"
    echo "  build-installer $host /dev/sdX"
    exit 0
  fi

  if [ ! -b "$dev" ]; then
    echo "Not a block device: $dev" >&2
    exit 1
  fi

  masterkey="$HOME/.config/sops/age/keys.txt"

  if [ -z "$hostkey_src" ] && [ "$host" = "$(hostname)" ] \
     && [ -f /etc/ssh/ssh_host_ed25519_key ]; then
    hostkey_src="/etc/ssh/ssh_host_ed25519_key"
  fi

  [ -f "$masterkey" ] || \
    echo "NOTE: master key $masterkey not found — sops edits on the installer won't work."
  if [ -n "$hostkey_src" ]; then
    echo "Host key for '$host' will be stashed from: $hostkey_src"
  else
    echo "NOTE: no host key for '$host' — install-system will use the interactive sops bootstrap."
  fi

  echo
  echo "About to ERASE and write to:"
  ${lsblk} -dno NAME,SIZE,MODEL,TRAN "$dev" || true
  echo
  read -r -p "Re-type the device path to confirm ($dev): " confirm
  [ "$confirm" = "$dev" ] || { echo "Mismatch — aborting."; exit 1; }

  echo ">> Unmounting any mounted partitions on $dev ..."
  for p in "$dev"?*; do sudo ${umount} "$p" 2>/dev/null || true; done

  echo ">> Flashing ISO to $dev ..."
  sudo dd if="$iso" of="$dev" bs=4M status=progress oflag=sync conv=fsync
  sudo ${partprobe} "$dev" || true
  sleep 2

  if [ -f "$masterkey" ] || [ -n "$hostkey_src" ]; then
    echo ">> Appending SOPSKEY partition for the sops secrets ..."
    parts_before=$(${lsblk} -lnpo NAME "$dev" | tail -n +2 | sort)
    sudo ${sgdisk} -e "$dev"
    sudo ${sgdisk} -n 0:0:0 -t 0:0700 -c 0:SOPSKEY "$dev"
    sudo ${partprobe} "$dev" || true
    sudo ${udevadm} settle || true
    sleep 2
    parts_after=$(${lsblk} -lnpo NAME "$dev" | tail -n +2 | sort)

    keypart=$(comm -13 <(echo "$parts_before") <(echo "$parts_after") | head -n1)
    if [ -z "$keypart" ]; then
      echo "Could not detect the new partition — aborting key write." >&2
      exit 1
    fi

    echo ">> Formatting $keypart as vfat (label SOPSKEY) ..."
    sudo ${mkfsvfat} -n SOPSKEY "$keypart"

    mnt=$(mktemp -d)
    sudo ${mount} "$keypart" "$mnt"

    if [ -f "$masterkey" ]; then
      sudo cp "$masterkey" "$mnt/keys.txt"
      echo ">> Master age key written."
    fi
    if [ -n "$hostkey_src" ]; then
      sudo mkdir -p "$mnt/hostkeys/$host"
      sudo cp "$hostkey_src" "$mnt/hostkeys/$host/ssh_host_ed25519_key"
      [ -f "''${hostkey_src}.pub" ] && \
        sudo cp "''${hostkey_src}.pub" "$mnt/hostkeys/$host/ssh_host_ed25519_key.pub"
      echo ">> Host key for '$host' written."
    fi

    sudo sync
    sudo ${umount} "$mnt"
    rmdir "$mnt"
    echo ">> SOPSKEY partition ready."
  fi

  echo
  echo "Done. Boot the target machine from $dev and run: install-system"
''
