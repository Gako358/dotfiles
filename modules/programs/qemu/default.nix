_: {
  flake.nixosModules.programs-qemu =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      cfg = config.program.qemu;
      win11-vm = pkgs.callPackage ./_win11-vm.nix { inherit pkgs; };
      rhuidean-vm = pkgs.callPackage ./_rhuidean-vm.nix { inherit pkgs; };
      mkDistroVm = name: url: pkgs.callPackage ./_distro-vm.nix { inherit pkgs name url; };
      guix-vm = mkDistroVm "guix" "https://guix.gnu.org/en/download/";
      arch-vm = mkDistroVm "arch" "https://archlinux.org/download/";
      cachyos-vm = mkDistroVm "cachyos" "https://cachyos.org/download/";
      fedora-vm = mkDistroVm "fedora" "https://fedoraproject.org/workstation/download";
    in
    {
      options.program.qemu = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Virt-Manager.";
        };
      };

      config = lib.mkIf (cfg.enable && config.environment.desktop.enable) {
        programs = {
          virt-manager.enable = true;
        };

        services = {
          spice-vdagentd.enable = true;
        };

        # For win quest, need to install the win spice and win virtio drivers
        # and set the qxl video setting to:
        # <model type='qxl' ram='131072' vram='131072' vgamem='32768' heads='1' primary='yes'/>

        virtualisation = {
          libvirtd = {
            enable = true;
            qemu = {
              package = pkgs.qemu_kvm;
              swtpm.enable = true;
              # These images comes default with qemu
              # ovmf.enable = true;
              # ovmf.packages = [ pkgs.OVMFFull.fd ];
            };
          };
          spiceUSBRedirection.enable = true;
        };
        environment = {
          systemPackages = with pkgs; [
            spice
            spice-vdagent
            spice-autorandr
            spice-gtk
            spice-protocol
            virt-viewer
            virtio-win
            win-spice
            virtio-win
            win11-vm
            rhuidean-vm
            guix-vm
            arch-vm
            cachyos-vm
            fedora-vm
          ];
          persistence."/persist" = {
            directories = [
              "/var/lib/libvirt/images"
            ];
            # Disks, firmware vars and TPM state for the *-vm scripts.
            users.merrinx.directories = [ ".local/share/vm" ];
          };
        };

        users.users.merrinx.extraGroups = [ "libvirtd" ];
      };
    };
}
