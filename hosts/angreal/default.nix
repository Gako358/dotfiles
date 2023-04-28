{
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ../common/core
    ../common/shell
  ];

  environment.etc."resolv.conf".enable = false;

  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      data-root = "/opt/containerd/";
    };
    # extraOptions = "--storage-driver=btrfs";
  };
  virtualisation.podman.enable = true;

  # Enable direnv until hm is restored
  environment.systemPackages = with pkgs; [
    direnv
    nix-direnv
    openssl
    docker-compose
  ];

  wsl = {
    enable = true;
    defaultUser = "nixos";
    startMenuLaunchers = true;
    nativeSystemd = true;

    wslConf.interop.appendWindowsPath = false;
    wslConf.network.generateResolvConf = false;

    # Enable native Docker support
    docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;
  };

  systemd.services.firewall.enable = false;
  systemd.services.systemd-resolved.enable = false;
  systemd.services.systemd-udevd.enable = false;

  networking = {
    hostName = "nixos";
    nameservers = [
      "172.30.205.7"
      "172.30.205.8"
      "172.30.205.5"
    ];
  };

  nix = {
    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      keep-outputs = true;
      keep-derivations = true;
    };
  };
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];
  nixpkgs.overlays = [
    (self: super: {nix-direnv = super.nix-direnv.override {enableFlakes = true;};})
  ];

  # Enable fonts
  # Create a symlink to the fonts directory in the user's home directory
  # $ ln -s /run/current-system/sw/share/X11/fonts ~/.local/share/fonts
  fonts.fontDir.enable = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  system.stateVersion = "23.05";
}
