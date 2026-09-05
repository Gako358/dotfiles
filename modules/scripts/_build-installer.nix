{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  lsblk = "${pkgs.util-linux}/bin/lsblk";
  readlink = "${pkgs.coreutils}/bin/readlink";
  sfdisk = "${pkgs.util-linux}/bin/sfdisk";
  blockdev = "${pkgs.util-linux}/bin/blockdev";
  umount = "${pkgs.util-linux}/bin/umount";
  partprobe = "${pkgs.parted}/bin/partprobe";
  jq = "${pkgs.jq}/bin/jq";
  mkfsvfat = "${pkgs.dosfstools}/bin/mkfs.vfat";
  mcopy = "${pkgs.mtools}/bin/mcopy";
  mmd = "${pkgs.mtools}/bin/mmd";
  cp = "${pkgs.coreutils}/bin/cp";
  dd = "${pkgs.coreutils}/bin/dd";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  rm = "${pkgs.coreutils}/bin/rm";
  truncate = "${pkgs.coreutils}/bin/truncate";
  stat = "${pkgs.coreutils}/bin/stat";
  id = "${pkgs.coreutils}/bin/id";
  udevadm = "${pkgs.systemd}/bin/udevadm";
  bash = "${pkgs.bash}/bin/bash";
in
pkgs.writeShellScriptBin "build-installer" ''
    set -euo pipefail

    usage() {
      echo "Usage: build-installer <host> [/dev/sdX] [host-key-path]"
      echo
      echo "  Builds a NixOS installer ISO for <host>."
      echo "  For SOPS-enabled hosts, supplying a USB device prepares an optional key partition."
      exit 1
    }

    host="''${1:-}"
    dev="''${2:-}"
    hostkey_src="''${3:-}"
    [ -n "$host" ] || usage
    if [[ ! "$host" =~ ^[A-Za-z0-9_-]+$ ]]; then
      echo "Invalid host name: '$host'" >&2
      exit 1
    fi

    if root=$(${git} rev-parse --show-toplevel 2>/dev/null); then :; else root="$PWD"; fi
    cd "$root"

    if [ ! -d "modules/hosts/$host" ]; then
      echo "Unknown host '$host'. Available hosts:" >&2
      ls modules/hosts >&2
      exit 1
    fi

    sops_enabled=$(nix eval --json ".#nixosConfigurations.$host.config.service.sops.enable")
    case "$sops_enabled" in
      true) sops=true ;;
      false) sops=false ;;
      *)
        echo "Expected service.sops.enable to evaluate to true or false, got: $sops_enabled" >&2
        exit 1
        ;;
    esac

    echo ">> Building installer ISO for '$host' (this can take a while)..."
    out=$(nix build ".#nixosConfigurations.installer-$host.config.system.build.isoImage" \
      --no-link --print-out-paths)
    iso_candidates=( "$out"/iso/*.iso )
    if [ "''${#iso_candidates[@]}" -ne 1 ] || [ ! -f "''${iso_candidates[0]}" ]; then
      echo "Expected exactly one ISO in $out/iso" >&2
      exit 1
    fi
    iso="''${iso_candidates[0]}"
    echo ">> ISO built: $iso"

    if [ "$sops" = false ] && [ -n "$hostkey_src" ]; then
      echo "A host key argument is only valid for SOPS-enabled hosts." >&2
      exit 1
    fi

    if [ -z "$dev" ]; then
      echo
      echo "No USB device given. To flash manually:"
      echo "  sudo dd if='$iso' of=/dev/sdX bs=4M status=progress oflag=sync"
      if [ "$sops" = true ]; then
        echo "For a SOPS-enabled host, re-run with a device to prepare its optional key partition:"
        echo "  build-installer $host /dev/sdX"
      fi
      exit 0
    fi

    if ! canonical_dev=$(${readlink} -f -- "$dev") || [ ! -b "$canonical_dev" ]; then
      echo "Not a block device: $dev" >&2
      exit 1
    fi
    target_type=$(${lsblk} -dnro TYPE "$canonical_dev")
    [ "$target_type" = disk ] || {
      echo "Target must be a whole disk (got TYPE=$target_type): $canonical_dev" >&2
      exit 1
    }

    image="$iso"
    flash_source="$iso"
    tmpdir=""
    cleanup() {
      if [ -n "$tmpdir" ] && ! ${rm} -rf "$tmpdir"; then
        echo "Warning: failed to clean up temporary directory $tmpdir" >&2
      fi
    }
    trap cleanup EXIT
    trap 'exit 130' INT TERM

    if [ "$sops" = true ]; then
      masterkey="$HOME/.config/sops/age/keys.txt"
      if [ -z "$hostkey_src" ] && [ "$host" = "$(hostname)" ] \
         && [ -f /etc/ssh/ssh_host_ed25519_key ]; then
        hostkey_src="/etc/ssh/ssh_host_ed25519_key"
      fi
      if [ -L "$masterkey" ] || { [ -e "$masterkey" ] && [ ! -f "$masterkey" ]; }; then
        echo "Master key is not a regular file (or is a symlink): $masterkey" >&2
        exit 1
      fi
      [ -f "$masterkey" ] || \
        echo "NOTE: master key $masterkey not found — sops edits on the installer won't work."
      if [ -n "$hostkey_src" ] && { [ -L "$hostkey_src" ] || [ ! -f "$hostkey_src" ]; }; then
        echo "Host key is not a regular file (or is a symlink): $hostkey_src" >&2
        exit 1
      fi
      if [ -n "$hostkey_src" ] && [ -L "''${hostkey_src}.pub" ]; then
        echo "Host public key is a symlink: ''${hostkey_src}.pub" >&2
        exit 1
      fi
      masterkey_snapshot=""
      hostkey_snapshot=""
      hostkey_pub_snapshot=""
      if [ -n "$hostkey_src" ]; then
        echo "Host key for '$host' will be stashed from: $hostkey_src"
      else
        echo "NOTE: no host key for '$host' — install-system will use the interactive sops bootstrap."
      fi
    fi

    echo
    echo "About to ERASE and write to:"
    ${lsblk} -dno NAME,SIZE,MODEL,TRAN "$canonical_dev" || true
    echo "Canonical target: $canonical_dev"
    if [ "$sops" = true ] && { [ -f "$masterkey" ] || [ -n "$hostkey_src" ]; }; then
      echo "!!! WARNING: plaintext SOPS master/host private keys will be embedded on this removable FAT filesystem."
      echo "!!! Possession of this USB grants access to those keys."
    fi
    echo
    read -r -p "Re-type the canonical device path to confirm ($canonical_dev): " confirm
    [ "$confirm" = "$canonical_dev" ] || { echo "Mismatch — aborting."; exit 1; }

    if ! sector_size=$(sudo ${blockdev} --getss "$canonical_dev"); then
      echo "Could not read target logical sector size with sudo." >&2
      exit 1
    fi
    [ "$sector_size" -eq 512 ] || { echo "Target must use 512-byte logical sectors." >&2; exit 1; }
    if ! device_bytes=$(sudo ${blockdev} --getsize64 "$canonical_dev"); then
      echo "Could not read target size with sudo." >&2
      exit 1
    fi
    if ! flash_bytes=$(${stat} -c %s "$flash_source"); then
      echo "Could not stat flash source: $flash_source" >&2
      exit 1
    fi
    [ "$flash_bytes" -le "$device_bytes" ] || {
      echo "Source image ($flash_bytes bytes) does not fit target ($device_bytes bytes)." >&2
      exit 1
    }
    target_identity=$(${stat} -c '%t:%T' "$canonical_dev")

    echo ">> Unmounting child partitions on $canonical_dev ..."
    mapfile -t children < <(${lsblk} -J -o PATH,TYPE,MOUNTPOINTS "$canonical_dev" | ${jq} -r '
      def descendants: .children? // [] | .[] as $child | $child, ($child | descendants);
      (.blockdevices[0] | descendants) | .path
    ')
    for ((i=''${#children[@]} - 1; i >= 0; i--)); do
      child="''${children[i]}"
      child_json=$(${lsblk} -J -o PATH,TYPE,MOUNTPOINTS "$child")
      if printf '%s\n' "$child_json" | ${jq} -e '.. | objects | (.mountpoints? // []) | any(. == "[SWAP]")' >/dev/null; then
        echo "Active swap detected on $child; run swapoff before flashing." >&2
        exit 1
      fi
      if printf '%s\n' "$child_json" | ${jq} -e '.. | objects | (.mountpoints? // []) | any(. != null and . != "")' >/dev/null; then
        sudo ${umount} "$child" || { echo "Failed to unmount $child" >&2; exit 1; }
      fi
    done
    remaining_json=$(${lsblk} -J -o PATH,TYPE,MOUNTPOINTS "$canonical_dev")
    if printf '%s\n' "$remaining_json" | ${jq} -e '.. | objects | (.mountpoints? // []) | any(. == "[SWAP]")' >/dev/null; then
      echo "Active swap detected on a target descendant; run swapoff before flashing." >&2
      exit 1
    fi
    if printf '%s\n' "$remaining_json" | ${jq} -e '.. | objects | (.mountpoints? // []) | any(. != null and . != "")' >/dev/null; then
      echo "A target descendant remains mounted or active; refusing to flash." >&2
      exit 1
    fi

    if [ "$sops" = true ] && { [ -f "$masterkey" ] || [ -n "$hostkey_src" ]; }; then
      tmp_base="''${XDG_RUNTIME_DIR:-}"
      if [ -z "$tmp_base" ] || [ ! -d "$tmp_base" ] || [ "$(${stat} -c %u "$tmp_base")" != "$(${id} -u)" ]; then
        tmp_base="''${TMPDIR:-/var/tmp}"
      fi
      tmpdir=$(${mktemp} -d "$tmp_base/build-installer.XXXXXX")
      chmod 700 "$tmpdir"
      snapshot_key() {
        local source=$1
        local snapshot=$2
        if ! ${cp} --no-dereference -- "$source" "$snapshot"; then
          echo "Could not snapshot key source: $source" >&2
          exit 1
        fi
        if [ -L "$snapshot" ] || [ ! -f "$snapshot" ]; then
          echo "Key snapshot is not a regular non-symlink file: $snapshot" >&2
          exit 1
        fi
        chmod 600 "$snapshot" || {
          echo "Could not protect key snapshot: $snapshot" >&2
          exit 1
        }
      }
      if [ -f "$masterkey" ]; then
        masterkey_snapshot="$tmpdir/masterkey"
        snapshot_key "$masterkey" "$masterkey_snapshot"
      fi
      if [ -n "$hostkey_src" ]; then
        hostkey_snapshot="$tmpdir/hostkey"
        snapshot_key "$hostkey_src" "$hostkey_snapshot"
        if [ -f "''${hostkey_src}.pub" ]; then
          hostkey_pub_snapshot="$tmpdir/hostkey.pub"
          snapshot_key "''${hostkey_src}.pub" "$hostkey_pub_snapshot"
        fi
      fi
      image="$tmpdir/installer.img"
      ${cp} --reflink=auto --sparse=always "$iso" "$image"
      chmod 600 "$image"

      iso_bytes=$(${stat} -c %s "$image")
      iso_sectors=$(( (iso_bytes + 511) / 512 ))
      ${sfdisk} --json "$image" | ${jq} -e --argjson iso_sectors "$iso_sectors" '
        .partitiontable.label == "dos" and
        (.partitiontable.partitions | length == 2) and
        (.partitiontable.partitions[0] |
          (.node | type == "string") and (.start == 0) and (.size == $iso_sectors) and
          ((.type | tostring | ascii_downcase) == "0")) and
        (.partitiontable.partitions[1] |
          (.node | type == "string") and (.start > 0) and (.size > 0) and
          ((.type | tostring | ascii_downcase) == "ef") and
          (.start + .size <= $iso_sectors))
      ' >/dev/null
      original_partitions=$(${sfdisk} --json "$image" | ${jq} -c '[.partitiontable.partitions[] | {start, size, type}]')

      key_start=$(( ((iso_sectors + 2047) / 2048) * 2048 ))
      key_sectors=$(( 64 * 1024 * 1024 / 512 ))
      image_bytes=$(( (key_start + key_sectors) * 512 ))
      [ "$image_bytes" -le "$device_bytes" ] || {
        echo "Target is too small for the ISO and 64MiB SOPSKEY partition." >&2
        exit 1
      }
      ${truncate} -s "$image_bytes" "$image"
      printf 'start=%s, size=%s, type=0e\n' "$key_start" "$key_sectors" |
        ${sfdisk} --append --no-reread --no-tell-kernel --wipe never -N 3 "$image"
      partition_json=$(${sfdisk} --json "$image")
      printf '%s\n' "$partition_json" | ${jq} -e \
        --argjson original "$original_partitions" \
        --argjson key_start "$key_start" \
        --argjson key_sectors "$key_sectors" '
        .partitiontable.label == "dos" and
        (.partitiontable.partitions | length == 3) and
        ([.partitiontable.partitions[0:2][] | {start, size, type}] == $original) and
        (.partitiontable.partitions[2] |
          (.start == $key_start) and
          (.size == $key_sectors) and
          ((.type | ascii_downcase) == "e" or (.type | ascii_downcase) == "0e"))
      ' >/dev/null

      ${mkfsvfat} -F 16 -n SOPSKEY --offset "$key_start" "$image" "$((key_sectors / 2))"
      key_offset=$((key_start * 512))
      if [ -n "$masterkey_snapshot" ]; then
        ${mcopy} -i "$image@@$key_offset" "$masterkey_snapshot" ::keys.txt
        echo ">> Master age key written."
      fi
      if [ -n "$hostkey_snapshot" ]; then
        ${mmd} -i "$image@@$key_offset" ::hostkeys
        ${mmd} -i "$image@@$key_offset" ::hostkeys/"$host"
        ${mcopy} -i "$image@@$key_offset" "$hostkey_snapshot" ::hostkeys/"$host"/ssh_host_ed25519_key
        if [ -n "$hostkey_pub_snapshot" ]; then
          ${mcopy} -i "$image@@$key_offset" "$hostkey_pub_snapshot" ::hostkeys/"$host"/ssh_host_ed25519_key.pub
        fi
        echo ">> Host key for '$host' written."
      fi
      echo ">> SOPSKEY partition ready."
      flash_source="$image"
    fi

    if ! current_dev=$(${readlink} -f -- "$canonical_dev") || [ "$current_dev" != "$canonical_dev" ] || [ ! -b "$current_dev" ] || [ "$(${lsblk} -dnro TYPE "$current_dev")" != disk ] || [ "$(${stat} -c '%t:%T' "$current_dev")" != "$target_identity" ]; then
      echo "Target device identity changed; refusing to flash." >&2
      exit 1
    fi
    flash_bytes=$(${stat} -c %s "$flash_source")
    [ "$flash_bytes" -le "$device_bytes" ] || { echo "Final source image no longer fits target." >&2; exit 1; }
    echo ">> Flashing ISO to $canonical_dev ..."
    immediate_json=$(${lsblk} -J -o PATH,TYPE,MOUNTPOINTS "$canonical_dev")
    if printf '%s\n' "$immediate_json" | ${jq} -e '.. | objects | (.mountpoints? // []) | any(. == "[SWAP]" or (. != null and . != ""))' >/dev/null; then
      echo "A target descendant became mounted or active; refusing to flash." >&2
      exit 1
    fi
    sudo ${bash} -s -- "$canonical_dev" "$target_identity" "$device_bytes" "$flash_source" "${dd}" "${blockdev}" "${stat}" <<'HELPER'
  set -euo pipefail
  canonical_dev=$1
  expected_identity=$2
  device_bytes=$3
  flash_source=$4
  dd=$5
  blockdev=$6
  stat=$7
  exec {target_fd}<>"$canonical_dev"
  opened_identity=$("$stat" -L -c '%t:%T' "/proc/self/fd/$target_fd")
  [ "$opened_identity" = "$expected_identity" ] || {
    echo "Opened target device identity changed; refusing to flash." >&2
    exit 1
  }
  opened_bytes=$("$blockdev" --getsize64 "/proc/self/fd/$target_fd")
  [ "$opened_bytes" -eq "$device_bytes" ] || {
    echo "Opened target device size changed; refusing to flash." >&2
    exit 1
  }
  [ "$opened_bytes" -ge $("$stat" -c %s "$flash_source") ] || {
    echo "Source image no longer fits opened target." >&2
    exit 1
  }
  "$dd" if="$flash_source" of=/dev/stdout bs=4M status=progress oflag=sync conv=fsync >&"$target_fd"
  HELPER
    sudo ${partprobe} "$canonical_dev" || true
    sudo ${udevadm} settle || true
    echo
    echo "Done. Boot the target machine from $dev and run: install-system"
''
