{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ../common/shell
  ];

  environment.etc."resolv.conf".enable = false;
  environment.systemPackages = with pkgs; [
    git
    zip
    curl
    wget
    cacert
    openssh
    lazygit
    xdotool
    ripgrep
    wgetpaste
    nix-index
    nodejs-16_x
    alejandra
  ];

  wsl = {
    enable = true;
    defaultUser = "nixos";
    startMenuLaunchers = true;
    nativeSystemd = true;

    wslConf.interop.appendWindowsPath = false;

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

  nixpkgs.config.allowUnfree = true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
