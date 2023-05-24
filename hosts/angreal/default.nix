{
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
    ../common
  ];

  users.users.nixos = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker"];
  };

  wsl = {
    enable = true;
    defaultUser = "nixos";
    nativeSystemd = true;
    docker-desktop.enable = true;

    # Behind corporate proxy
    wslConf = {
      network = {
        generateResolvConf = false;
      };
      interop = {
        appendWindowsPath = false;
      };
    };
  };

  # Set desktop environment
  desktop.environment = "wsl";

  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];

  networking = {
    dhcpcd.enable = false;
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

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  system.stateVersion = "23.05";
}
