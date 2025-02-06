{ config
, inputs
, lib
, pkgs
, ...
}: {
  imports = [
    ./programs
    ./services
  ];
  # NixOS Network Configuration
  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    extraHosts = ''
      127.0.0.1 tuathaan
      104.199.65.124 ap-gew4.spotify.com
    '';
  };
  # NixOS enable Flakes
  nix = {
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
    # Weekly garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    # Trusted users
    settings.trusted-users = [ "root" "merrinx" "@wheel" ];

    # Enable optimisation
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
  };
  # Timezone and locale
  time.timeZone = "Europe/Oslo";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
  ];
  console.useXkbConfig = true;
  programs.dconf.enable = true;

  # Set the default editor
  environment.variables.EDITOR = "nvim";
  security = {
    rtkit.enable = true;
    pam.services.swaylock = { };
  };

  # Enable hardware support
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    keyboard.zsa.enable = true;
    enableAllFirmware = true;
  };
  # Set default shell to fish global
  users.defaultUserShell = pkgs.fish;
  # Enable proprietary software
  nixpkgs.config.allowUnfree = true;
  system.stateVersion = "24.11";
}
