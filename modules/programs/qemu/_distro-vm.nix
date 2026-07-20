{
  pkgs,
  name,
  url,
}:
let
  qemu = "${pkgs.qemu_kvm}/bin/qemu-system-x86_64";
  qemu-img = "${pkgs.qemu_kvm}/bin/qemu-img";
  ovmf = pkgs.OVMFFull.fd;
in
pkgs.writeShellScriptBin "${name}-vm" ''
  set -euo pipefail

  usage() {
    echo "Usage: ${name}-vm [/path/to/installer.iso]"
    echo
    echo "  First run: pass the ${name} installer ISO to create the VM."
    echo "  Later runs: no argument needed, boots the installed disk."
    echo "  ${name}-vm --installer /path/to.iso boots the installer against the existing disk."
    echo "  ${name}-vm --reset deletes the VM state to start over."
    echo
    echo "  Get the ISO from ${url}"
    echo "  State: ~/.local/share/vm/${name} (disk, firmware vars)."
    exit 1
  }

  dir="''${XDG_DATA_HOME:-$HOME/.local/share}/vm/${name}"
  disk="$dir/${name}.qcow2"
  vars="$dir/OVMF_VARS.fd"
  iso=""

  case "''${1:-}" in
    -h|--help) usage ;;
    --installer) iso="''${2:-}"; [ -n "$iso" ] || usage ;;
    --reset)
      read -r -p "Delete $dir and start over? [y/N] " ans
      [ "$ans" = "y" ] && rm -rf "$dir" && echo "Removed. Re-run with the ISO to reinstall."
      exit 0
      ;;
    *) iso="''${1:-}" ;;
  esac

  mkdir -p "$dir"

  if [ ! -f "$disk" ] && [ -z "$iso" ]; then
    echo "No VM yet - the first run needs the ${name} installer ISO." >&2
    usage
  fi

  if [ -n "$iso" ] && [ ! -f "$iso" ]; then
    echo "No such file: $iso" >&2
    exit 1
  fi

  if [ ! -f "$disk" ]; then
    echo ">> Creating 64G disk and firmware vars ..."
    ${qemu-img} create -f qcow2 "$disk" 64G
    install -m644 ${ovmf}/FV/OVMF_VARS.fd "$vars"
  fi

  mem=$(( $(awk '/MemTotal/ {print $2}' /proc/meminfo) / 2048 ))
  cpus=$(( $(nproc) / 2 ))

  extra=()
  if [ -n "$iso" ]; then
    extra+=( -device ahci,id=ahci
             -drive if=none,id=cd,media=cdrom,file="$iso"
             -device ide-cd,drive=cd,bus=ahci.0,bootindex=0 )
    echo ">> Booting installer. Install onto /dev/vda (UEFI)."
  fi

  exec ${qemu} \
    -enable-kvm -machine q35,smm=on -cpu host -m "$mem" -smp "$cpus" \
    -global driver=cfi.pflash01,property=secure,value=on \
    -drive if=pflash,format=raw,unit=0,readonly=on,file=${ovmf}/FV/OVMF_CODE.fd \
    -drive if=pflash,format=raw,unit=1,file="$vars" \
    -drive if=virtio,file="$disk",discard=unmap \
    -nic user,model=virtio-net-pci \
    -device qemu-xhci -device usb-tablet \
    -device virtio-vga,xres=2560,yres=1440 \
    -display gtk,full-screen=on \
    "''${extra[@]}"
''
