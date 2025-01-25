{ pkgs, ... }: {
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
        ovmf.enable = true;
        ovmf.packages = [ pkgs.OVMFFull.fd ];
      };
    };
    spiceUSBRedirection.enable = true;
  };
  environment.systemPackages = with pkgs; [
    spice
    spice-vdagent
    spice-autorandr
    spice-gtk
    spice-protocol
    virt-viewer
    virtio-win
    win-spice
    win-virtio
  ];
}
