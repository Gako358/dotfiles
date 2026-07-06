{ pkgs, ... }:
let
  qemu = "${pkgs.qemu_kvm}/bin/qemu-system-x86_64";
  qemu-img = "${pkgs.qemu_kvm}/bin/qemu-img";
  git = "${pkgs.git}/bin/git";
  ssh-keygen = "${pkgs.openssh}/bin/ssh-keygen";
  mkfsvfat = "${pkgs.dosfstools}/bin/mkfs.vfat";
  mcopy = "${pkgs.mtools}/bin/mcopy";
  mmd = "${pkgs.mtools}/bin/mmd";
  ovmf = pkgs.OVMFFull.fd;
in
pkgs.writeShellScriptBin "rhuidean-vm" ''
  set -euo pipefail

  usage() {
    echo "Usage: rhuidean-vm [--installer | --reset]"
    echo
    echo "  No disk yet   -> builds the rhuidean installer ISO + SOPSKEY image"
    echo "                   and boots the installer (run install-system inside)."
    echo "  Disk exists   -> boots the installed system."
    echo "  --installer   -> boot the installer again against the existing disk."
    echo "  --reset       -> delete all VM state and start over."
    echo
    echo "  Run from inside the dotfiles checkout (needs the flake for nix build)."
    echo "  State: ~/.local/share/vm/rhuidean (disk, firmware vars, SOPSKEY)."
    exit 1
  }

  dir="''${XDG_DATA_HOME:-$HOME/.local/share}/vm/rhuidean"
  disk="$dir/rhuidean.qcow2"
  vars="$dir/OVMF_VARS.fd"
  sopskey="$dir/sopskey.img"
  install_mode=false

  case "''${1:-}" in
    -h|--help) usage ;;
    --installer) install_mode=true ;;
    --reset)
      read -r -p "Delete $dir and start over? [y/N] " ans
      [ "$ans" = "y" ] && rm -rf "$dir" && echo "Removed. Re-run to reinstall."
      exit 0
      ;;
    "") ;;
    *) usage ;;
  esac

  mkdir -p "$dir"
  [ -f "$disk" ] || install_mode=true

  mem=$(( $(awk '/MemTotal/ {print $2}' /proc/meminfo) / 2048 ))
  cpus=$(( $(nproc) / 2 ))

  extra=()
  if $install_mode; then
    if root=$(${git} rev-parse --show-toplevel 2>/dev/null); then :; else root="$PWD"; fi
    if [ ! -d "$root/modules/hosts/rhuidean" ]; then
      echo "Not inside the dotfiles checkout - cd there and re-run." >&2
      exit 1
    fi

    echo ">> Building installer ISO for rhuidean (this can take a while) ..."
    out=$(nix build "$root#nixosConfigurations.installer-rhuidean.config.system.build.isoImage" \
      --no-link --print-out-paths)
    iso=$(echo "$out"/iso/*.iso)
    echo ">> ISO built: $iso"

    if [ ! -f "$sopskey" ]; then
      masterkey="$HOME/.config/sops/age/keys.txt"
      [ -f "$masterkey" ] || { echo "Master age key not found: $masterkey" >&2; exit 1; }

      echo ">> Building SOPSKEY image (master age key + throwaway host key) ..."
      tmp=$(mktemp -d)
      trap 'rm -rf "$tmp"' EXIT
      ${ssh-keygen} -q -t ed25519 -N "" -C "root@rhuidean" -f "$tmp/hostkey"

      truncate -s 64M "$sopskey"
      ${mkfsvfat} -n SOPSKEY "$sopskey"
      ${mcopy} -i "$sopskey" "$masterkey" ::keys.txt
      ${mmd} -i "$sopskey" ::hostkeys ::hostkeys/rhuidean
      ${mcopy} -i "$sopskey" "$tmp/hostkey" ::hostkeys/rhuidean/ssh_host_ed25519_key
      ${mcopy} -i "$sopskey" "$tmp/hostkey.pub" ::hostkeys/rhuidean/ssh_host_ed25519_key.pub
    fi

    if [ ! -f "$disk" ]; then
      echo ">> Creating 64G disk and firmware vars (Setup Mode, nothing enforced) ..."
      ${qemu-img} create -f qcow2 "$disk" 64G
      install -m644 ${ovmf}/FV/OVMF_VARS.fd "$vars"
    fi

    extra+=( -device ahci,id=ahci
             -drive if=none,id=cd,media=cdrom,file="$iso"
             -device ide-cd,drive=cd,bus=ahci.0,bootindex=0
             -drive if=none,id=sopskey,format=raw,file="$sopskey"
             -device usb-storage,bus=xhci.0,drive=sopskey )

    echo ">> Booting installer. Inside the guest run: install-system, then poweroff."
    echo ">> Next rhuidean-vm run boots the installed disk; enroll Secure Boot with:"
    echo ">>   sudo sbctl enroll-keys --microsoft && sudo reboot"
  fi

  exec ${qemu} \
    -enable-kvm -machine q35,smm=on -cpu host -m "$mem" -smp "$cpus" \
    -global driver=cfi.pflash01,property=secure,value=on \
    -drive if=pflash,format=raw,unit=0,readonly=on,file=${ovmf}/FV/OVMF_CODE.fd \
    -drive if=pflash,format=raw,unit=1,file="$vars" \
    -drive if=virtio,file="$disk" \
    -device qemu-xhci,id=xhci -device usb-tablet \
    -device virtio-vga-gl,xres=2560,yres=1440 \
    -display gtk,gl=on,full-screen=on \
    "''${extra[@]}"
''
