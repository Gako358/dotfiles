{ pkgs, ... }:
let
  qemu = "${pkgs.qemu_kvm}/bin/qemu-system-x86_64";
  qemu-img = "${pkgs.qemu_kvm}/bin/qemu-img";
  swtpm = "${pkgs.swtpm}/bin/swtpm";
  ovmf = pkgs.OVMFFull.fd;
  virtio-win-iso = pkgs.virtio-win.src;
in
pkgs.writeShellScriptBin "win11-vm" ''
  set -euo pipefail

  usage() {
    echo "Usage: win11-vm [/path/to/win11.iso]"
    echo
    echo "  First run: pass the Windows 11 installer ISO to create the VM."
    echo "  Later runs: no argument needed, boots the installed disk."
    echo "  win11-vm --reset deletes the VM state to start over."
    echo
    echo "  State: ~/.local/share/vm/win11 (disk, firmware vars, TPM)."
    exit 1
  }

  dir="''${XDG_DATA_HOME:-$HOME/.local/share}/vm/win11"
  disk="$dir/win11.qcow2"
  vars="$dir/OVMF_VARS.fd"
  iso=""

  case "''${1:-}" in
    -h|--help) usage ;;
    --reset)
      read -r -p "Delete $dir and start over? [y/N] " ans
      [ "$ans" = "y" ] && rm -rf "$dir" && echo "Removed. Re-run with the ISO to reinstall."
      exit 0
      ;;
    *) iso="''${1:-}" ;;
  esac

  mkdir -p "$dir/tpm"

  if [ ! -f "$disk" ]; then
    if [ -z "$iso" ]; then
      echo "No VM yet - the first run needs the Windows 11 installer ISO." >&2
      usage
    fi
    [ -f "$iso" ] || { echo "No such file: $iso" >&2; exit 1; }
    echo ">> Creating 128G disk and firmware vars ..."
    ${qemu-img} create -f qcow2 "$disk" 128G
    install -m644 ${ovmf}/FV/OVMF_VARS.fd "$vars"
  fi

  mem=$(( $(awk '/MemTotal/ {print $2}' /proc/meminfo) / 2048 ))
  cpus=$(( $(nproc) / 2 ))

  # Windows 11 requires TPM 2.0; qemu talks to this emulator socket.
  # --terminate makes swtpm exit when qemu disconnects.
  ${swtpm} socket --tpm2 --daemon --terminate \
    --tpmstate dir="$dir/tpm" \
    --ctrl type=unixio,path="$dir/tpm/swtpm.sock"

  extra=()
  if [ -n "$iso" ]; then
    extra+=( -drive if=none,id=wincd,media=cdrom,file="$iso"
             -device ide-cd,drive=wincd,bus=ahci.1,bootindex=0 )
    echo ">> Booting installer. The disk shows up as NVMe - no drivers needed."
    echo ">> After install, run virtio-win-gt-x64.msi from the attached CD for guest tools."
  fi

  exec ${qemu} \
    -enable-kvm -machine q35,smm=on -cpu host -m "$mem" -smp "$cpus" \
    -global driver=cfi.pflash01,property=secure,value=on \
    -drive if=pflash,format=raw,unit=0,readonly=on,file=${ovmf}/FV/OVMF_CODE.fd \
    -drive if=pflash,format=raw,unit=1,file="$vars" \
    -chardev socket,id=chrtpm,path="$dir/tpm/swtpm.sock" \
    -tpmdev emulator,id=tpm0,chardev=chrtpm \
    -device tpm-tis,tpmdev=tpm0 \
    -drive if=none,id=os,format=qcow2,file="$disk",discard=unmap \
    -device nvme,drive=os,serial=win11os \
    -device ahci,id=ahci \
    -drive if=none,id=drivers,media=cdrom,file=${virtio-win-iso} \
    -device ide-cd,drive=drivers,bus=ahci.0 \
    -nic user,model=e1000e \
    -audiodev pipewire,id=snd0 \
    -device intel-hda -device hda-duplex,audiodev=snd0 \
    -device qemu-xhci -device usb-tablet \
    -device virtio-vga,xres=2560,yres=1440 \
    -display gtk,full-screen=on \
    "''${extra[@]}"
''
