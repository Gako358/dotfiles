{
  pkgs,
  lib,
  config,
  ...
}: {
  virtualisation.docker = {
    enable = true;
    daemon.settings = {
    data-root = "/opt/containerd/";
    };
    # extraOptions = "--storage-driver=btrfs";
  };

  virtualisation.libvirtd.enable = true;
}
