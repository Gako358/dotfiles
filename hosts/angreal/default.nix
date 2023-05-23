{
  lib,
  pkgs,
  config,
  inputs,
  ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ../common/core
    ../common/shell
  ];

  # WSL uses its own bootloader, so disable the NixOS one
  boot = {
    initrd.enable = false;
    kernel.enable = false;
    loader.grub.enable = false;
    modprobeConfig.enable = false;
  };
  systemd.build.installBootLoader = "${pkgs.coreutils}/bin/true";

  # Disable power management in WSL
  powerManagement.enable = false;

  # Default user will not have root password
  security.sudo.wheelNeedsPassword = false;

  users.users.nixos = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker"];
  };

  # Otherwise WSL will fail to login as root with "initgroups: invalid argument"
  users.users.root.extraGroups = ["root"];

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "nixos";
    startMenuLaunchers = true;
    nativeSystemd = true;
    docker-desktop.enable = true;
  };

  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8 UTF-8"
  ];

  # WSL does not support virtual consoles
  console.enable = false;

  # Enable hardware acceleration
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  environment.etc = {
    "resolv.conf".enable = false;
    "hosts".enable = false;
  };

  networking = {
    dhcpcd.enable = false;
    nameservers = [
      "172.30.205.7"
      "172.30.205.8"
      "172.30.205.5"
    ];
  };

  virtualisation.docker.rootless = {
    enable = true;
    rootless.setSocketVariable = true;
  };

  # Enable properiety packages
  nixpkgs.config.allowUnfree = true;

  # Enable fonts
  fonts.fontDir.enable = true;

  # Packages to install in the system profile
  environment.systemPackages = with pkgs; [
    direnv
    openssl
    coreutils
    nix-direnv
    docker-compose
  ];

  services.openssh = {
    enable = true;
    settings.passwordAuthentication = false;
  };

  systemd.services = {
    firewall.enable = false;
    systemd-resolved.enable = false;
    systemd-udevd.enable = false;
    docker-desktop-proxy = {
      description = "Docker Desktop proxy";
      script = ''
        ${config.wsl.wslConf.automount.root}/wsl/docker-desktop/docker-desktop-user-distro proxy --docker-desktop-root ${config.wsl.wslConf.automount.root}/wsl/docker-desktop
      '';
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };
    users.groups.docker.members = [
      config.wsl.defaultUser
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

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  system.stateVersion = "23.05";
}
